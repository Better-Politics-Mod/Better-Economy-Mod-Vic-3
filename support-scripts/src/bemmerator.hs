{-# LANGUAGE LambdaCase#-}
module Main where

import PdxParser
import BuyPackages
import System.FilePath
import System.Directory
import Control.Applicative
import System.Process
import Data.List
import Data.Maybe

-- DEFINES
targetBasePrice = 100
employmentDivisor :: Double
employmentDivisor = 10
throughputDivisor = 10
baseTradeCap = 100
nominalFactor = 15
priceRange = 0.9
diffMaxFactor = 3
wealthLevels = 200
--

baseCosts :: PdxValue -> [(String, Double)]
baseCosts = go Nothing
  where
    go :: Maybe String -> PdxValue -> [(String, Double)]
    go parentKey v = case v of
        PdxPair (key, _, PdxArray xs) -> concatMap (go (Just key)) xs
        PdxPair ("cost", _, PdxNumber n) ->
            case parentKey of
                Just parent -> [(parent, n)]
                Nothing -> []
        PdxPair (_, _, val) -> go parentKey val
        PdxArray xs -> concatMap (go parentKey) xs
        _ -> []

genGoods :: PdxValue -> PdxValue
genGoods v = case v of
    PdxPair ("cost", op, _) -> PdxPair ("cost", op, PdxNumber targetBasePrice)
    PdxPair ("traded_quantity", op, _) -> PdxPair ("traded_quantity", op, PdxNumber $ baseTradeCap / targetBasePrice * nominalFactor)
    PdxPair (key, op, val) -> PdxPair (key, op, genGoods val)
    PdxArray xs -> PdxArray (map genGoods xs)
    other -> other

genPMs :: [(String, Double)] -> PdxValue -> PdxValue
genPMs costMap = go False
  where
    go inScaled v = case v of
        PdxPair (key, op, PdxNumber n) -> 
            PdxPair (key, op, PdxNumber $ transform key n inScaled)
        PdxPair (key, op, val) -> 
            PdxPair (key, op, go (inScaled || isScaled key) val)
        PdxArray xs -> PdxArray (map (go inScaled) xs)
        other -> other
    
    transform key n inScaled
        | key == "building_subsistence_output_add" = n * nominalFactor
        | otherwise = case snd <$> runParser prodModifierP key of
            Just goodName -> case lookup goodName costMap of
                Just basePrice -> basePrice / targetBasePrice * n / throughputDivisor * nominalFactor
                Nothing -> n
            Nothing -> case runParser employmentModifierP key of
                Just _ -> n / employmentDivisor
                Nothing -> if inScaled then n / throughputDivisor else n
    
    isScaled k = k `elem` ["level_scaled", "workforce_scaled"]
    
    prodModifierP = (\_ _ key -> key) 
        <$> stringP "goods_" <*> (stringP "output_" <|> stringP "input_") <*> spanUntilP "_add"
    
    employmentModifierP = (\_ key _ -> key) 
        <$> stringP "building_employment_" <*> spanP (/= '_') <*> stringP "_add"

genStateRegions :: PdxValue -> PdxValue
genStateRegions v = case v of
    PdxPair (key, op, PdxNumber n) 
        | "building_" `isPrefixOf` key || "arable_land" == key -> PdxPair (key, op, PdxNumber $ n * throughputDivisor)
    PdxPair (key, op, val) -> PdxPair (key, op, genStateRegions  val)
    PdxArray xs ->  PdxArray (map genStateRegions xs)
    other -> other

genHistory :: PdxValue -> PdxValue
genHistory = go Nothing
  where
    go building v = case v of
        PdxPair ("create_building", op, val@(PdxArray xs)) -> PdxPair ("create_building", op, go (setBuilding xs) val)
        PdxPair ("levels", op, PdxNumber n) 
            | building == Just "building_trade_center" -> PdxPair ("levels", op, PdxNumber 0)
            | building /= Just "building_barracks" -> PdxPair ("levels", op, PdxNumber $ n * throughputDivisor)
        PdxPair (key, op, val) -> PdxPair (key, op, go building val)
        PdxArray xs -> PdxArray (map (go building) xs)
        other -> other
    setBuilding [] = Nothing
    setBuilding (PdxPair ("building", _, PdxQString s) : _) = Just s
    setBuilding (_ : rest) = setBuilding rest

genPoptypes :: PdxValue -> PdxValue
genPoptypes v = case v of
    PdxPair ("dependent_wage", op, PdxNumber n) -> PdxPair ("dependent_wage", op, PdxNumber $ n * nominalFactor)
    PdxPair ("laborers", op, PdxArray n) -> PdxPair ("laborers", op, PdxArray (map genPoptypes $ filter (\case PdxPair (x, _, _) -> x /= "unemployment_wealth"; _ -> True) n))
    PdxPair (key, op, val) -> PdxPair (key, op, genPoptypes val)
    PdxArray xs -> PdxArray (map genPoptypes xs)
    other -> other

genBuildingGroups :: PdxValue -> PdxValue
genBuildingGroups v = case v of
    PdxPair (key, op, PdxNumber n) 
        | key `elem` ["cash_reserves_max", "urbanization", "infrastructure_usage_per_level"] -> PdxPair (key, op, PdxNumber $ n / throughputDivisor)
    PdxPair (key, op, val) -> PdxPair (key, op, genBuildingGroups val)
    PdxArray xs -> PdxArray (map genBuildingGroups xs)
    other -> other

genLaws :: PdxValue -> PdxValue
genLaws v = case v of
    PdxPair (key, op, PdxNumber n)
        | key `elem` ["tax_per_capita_add", "tax_land_add"] -> PdxPair (key, op, PdxNumber $ n * nominalFactor - n)
    PdxPair (key, op, val) -> 
        case genLaws val of
            PdxArray [] -> PdxArray []
            transformed -> PdxPair (key, op, transformed)
    PdxArray xs -> 
        let transformed = filter (\case PdxArray [] -> False; _ -> True) (map genLaws xs)
        in PdxArray transformed
    _ -> PdxArray []

genDefines :: PdxValue -> PdxValue
genDefines v = case v of
    PdxPair (key, op, val) -> 
        case lookup key defines of
            Just rule -> PdxPair (key, op, rule val)
            Nothing -> case genDefines val of
                PdxArray [] -> PdxArray []
                transformed -> PdxPair (key, op, transformed)
    PdxArray xs -> 
        let transformed = filter (\case PdxArray [] -> False; _ -> True) (map genDefines xs)
        in PdxArray transformed
    _ -> PdxArray []
  where
    defines = 
        [ ("COUNTRY_GDP_MODIFIER_DIVISOR", \(PdxNumber n) -> PdxNumber $ n * nominalFactor)
        , ("PRICE_RANGE", const $ PdxNumber priceRange)
        , ("BUY_SELL_DIFF_AT_MAX_FACTOR", const $ PdxNumber diffMaxFactor)
        , ("GOODS_SHORTAGE_PENALTY_THRESHOLD", const $ PdxNumber $ 1 / diffMaxFactor)
        , ("TRADE_CENTER_ADVANTAGE_PRICE_MULTIPLIER", const $ PdxNumber 0.3)
        , ("COUNTRY_MIN_CREDIT_BASE", \(PdxNumber n) -> PdxNumber $ n * nominalFactor)
        , ("QUALITY_OF_LIFE_MAX", const $ PdxNumber wealthLevels)
        , ("EARNINGS_ABSOLUTE_HIGH_THRESHOLD", \(PdxNumber n) -> PdxNumber $ n * nominalFactor * employmentDivisor / throughputDivisor)
        , ("EARNINGS_ABSOLUTE_LOW_THRESHOLD", \(PdxNumber n) -> PdxNumber $ n * nominalFactor * employmentDivisor / throughputDivisor)
        , ("PRIVATIZATION_PER_LEVEL_COST", const $ PdxNumber 0.1)
        , ("BUILDING_PRIVATIZATION_CHANCE", const $ PdxNumber 100)
        , ("AUTONOMOUS_TRADE_MIN_DESIRABILITY_PER_QUANTITY_TO_MAINTAIN_TRADE", const $ PdxNumber $ 5 * nominalFactor)
        , ("AUTONOMOUS_TRADE_MIN_DESIRABILITY_PER_QUANTITY_TO_INCREASE_TRADE", const $ PdxNumber $ 10 * nominalFactor)
        , ("CONSTRUCTION_CAMP_BUILDING", const $ PdxQString "building_consec_dummy")
        ]

main :: IO ()
main = do 
    bemCommon <- (</> "better-economy-mod/common") . init <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
    vic3Common <- (</> ".local/share/Steam/steamapps/common/Victoria 3/game/common") <$> getHomeDirectory
    Just goods <- parseFile (vic3Common </> "goods/00_goods.txt")
    processDirectory Replace ["00", "05", "08", "13"] (genPMs $ baseCosts goods) "production_methods"
    processDirectory Replace [] genGoods "goods"
    processDirectory Replace [] genPoptypes "pop_types"
    processDirectory None ["99"] genStateRegions "../map_data/state_regions"
    processDirectory None [] genHistory "history/buildings"
    processDirectory Replace [] genBuildingGroups "building_groups"
    processDirectory Inject [] genLaws "laws"
    processDirectory None [] genDefines "defines"
    processDirectory ReplaceOrCreate [] (const $ genBuyPackages wealthLevels) "buy_packages"
    writeFile (bemCommon </> "script_values/bem_trade_values.txt") $ toPdxScript $ PdxArray [PdxPair ("used_trade_capacity", "=", PdxArray [PdxPair ("value", "=", PdxUString "state_trade"), PdxPair ("divide", "=", PdxNumber $ targetBasePrice / baseTradeCap * nominalFactor)])]