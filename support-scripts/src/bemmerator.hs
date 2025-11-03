{-# LANGUAGE LambdaCase#-}
module Main where

import PdxParser
import System.FilePath
import System.Directory
import Control.Applicative
import System.Process
import Data.List

targetBasePrice = 100
employmentDivisor = 10
throughputDivisor = 10
baseTradeCap = 100
nominalFactor = 15

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

normalizeGoods :: PdxValue -> PdxValue
normalizeGoods v = case v of
    PdxPair ("cost", op, _) -> PdxPair ("cost", op, PdxNumber targetBasePrice)
    PdxPair ("traded_quantity", op, _) -> PdxPair ("traded_quantity", op, PdxNumber $ baseTradeCap / targetBasePrice * nominalFactor)
    PdxPair (key, op, val) -> PdxPair (key, op, normalizeGoods val)
    PdxArray xs -> PdxArray (map normalizeGoods xs)
    other -> other

normalizePMs :: [(String, Double)] -> PdxValue -> PdxValue
normalizePMs costMap = go False
  where
    go inModifierBlock v = case v of
        PdxPair (key, op, PdxNumber n) -> 
            PdxPair (key, op, PdxNumber $ transform key n inModifierBlock)
        PdxPair (key, op, val) -> 
            PdxPair (key, op, go (inModifierBlock || isModifierBlock key) val)
        PdxArray xs -> PdxArray (map (go inModifierBlock) xs)
        other -> other
    
    transform key n inMod
        | key == "building_subsistence_output_add" = n * nominalFactor
        | otherwise = case snd <$> runParser prodModifierP key of
            Just goodName -> case lookup goodName costMap of
                Just basePrice -> basePrice / targetBasePrice * n / throughputDivisor * nominalFactor
                Nothing -> n
            Nothing -> case runParser employmentModifierP key of
                Just _ -> n / employmentDivisor
                Nothing -> if inMod then n / throughputDivisor else n
    
    isModifierBlock k = k `elem` ["building_modifiers", "state_modifiers", "country_modifiers"]
    
    prodModifierP = (\_ _ key -> key) 
        <$> stringP "goods_" <*> (stringP "output_" <|> stringP "input_") <*> spanUntilP "_add"
    
    employmentModifierP = (\_ key _ -> key) 
        <$> stringP "building_employment_" <*> spanP (/= '_') <*> stringP "_add"

normalizePotentials :: PdxValue -> PdxValue
normalizePotentials v = case v of
    PdxPair (key, op, PdxNumber n) 
        | "bg_" `isPrefixOf` key || "arable_land" == key -> PdxPair (key, op, PdxNumber $ n * throughputDivisor)
    PdxPair (key, op, val) -> PdxPair (key, op, normalizePotentials  val)
    PdxArray xs ->  PdxArray (map normalizePotentials xs)
    other -> other

normalizeHistory :: PdxValue -> PdxValue
normalizeHistory = go Nothing
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

normalizeBuildingGroups :: PdxValue -> PdxValue
normalizeBuildingGroups v = case v of
    PdxPair (key, op, PdxNumber n) 
        | key `elem` ["cash_reserves_max", "urbanization", "infrastructure_usage_per_level"] -> PdxPair (key, op, PdxNumber $ n / throughputDivisor)
    PdxPair (key, op, val) -> PdxPair (key, op, normalizeBuildingGroups val)
    PdxArray xs -> PdxArray (map normalizeBuildingGroups xs)
    other -> other

main :: IO ()
main = do 
    bemCommon <- (</> "better-economy-mod/common") . init <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
    vic3Common <- (</> ".local/share/Steam/steamapps/common/Victoria 3/game/common") <$> getHomeDirectory
    Just goods <- parseFile (vic3Common </> "goods/00_goods.txt")
    processDirectory ["00", "05", "08", "13"] (normalizePMs $ baseCosts goods) (vic3Common </> "production_methods") (bemCommon </> "production_methods")
    writeFile (bemCommon </> "goods/zz_bem_goods.txt") (toPdxScript $ normalizeGoods goods)
    processDirectory [] genPoptypes (vic3Common </> "pop_types") (bemCommon </> "pop_types")
    processDirectory ["99"] normalizePotentials (vic3Common </> "../map_data/state_regions") (bemCommon </> "../map_data/state_regions")
    processDirectory [] normalizeHistory (vic3Common </> "history/buildings") (bemCommon </> "history/buildings")
    Just buildingGroups <- parseFile (vic3Common </> "building_groups/00_building_groups.txt")
    writeFile (bemCommon </> "building_groups/00_building_groups.txt") $ toPdxScript $ normalizeBuildingGroups buildingGroups
    writeFile (bemCommon </> "script_values/bem_trade_values.txt") $ toPdxScript $ PdxArray [PdxPair ("used_trade_capacity", "=", PdxArray [PdxPair ("value", "=", PdxUString "state_trade"), PdxPair ("divide", "=", PdxNumber $ targetBasePrice / baseTradeCap * nominalFactor)])]