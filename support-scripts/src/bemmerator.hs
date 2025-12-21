{-# LANGUAGE LambdaCase#-}
module Main where

import PdxParser
import System.FilePath
import System.Directory
import Control.Applicative
import System.Process
import Data.List
import Text.Printf

-- DEFINES --
basePrice = 100
employmentDivisor = 10
throughputDivisor = 10
baseTradeCap = 100
nominalFactor = 15
priceRange = 0.9
maxDiffFactor = 3
wealthLevels = 200
----

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
    PdxPair ("cost", op, _) -> PdxPair ("cost", op, PdxNumber basePrice)
    PdxPair ("traded_quantity", op, _) -> PdxPair ("traded_quantity", op, PdxNumber $ baseTradeCap / basePrice * nominalFactor)
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
                Just oBasePrice -> oBasePrice / basePrice * n / throughputDivisor * nominalFactor
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
            -- | building == Just "building_trade_center" -> PdxPair ("levels", op, PdxNumber 0)
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

genBuildings :: PdxValue -> PdxValue
genBuildings (PdxArray xs) = PdxArray $ map process xs
  where
    process (PdxPair (k, op, PdxArray fs)) = 
        PdxPair (k, op, PdxArray $ [f | f@(PdxPair (fk, _, _)) <- fs, fk /= "can_build_private"] 
            ++ [PdxPair ("can_build_private", "=", PdxArray [PdxPair ("always", "=", PdxBool False)])])
    process other = other
genBuildings other = other

genBuildingGroups :: PdxValue -> PdxValue
genBuildingGroups v = case v of
    PdxPair (key, op, PdxNumber n) 
        | key == "infrastructure_usage_per_level" -> PdxPair (key, op, PdxNumber $ n / throughputDivisor)
        | key == "cash_reserves_max" -> PdxPair (key, op, PdxNumber $ n / throughputDivisor * nominalFactor * 2)
    PdxPair (key, op, val) -> PdxPair (key, op, genBuildingGroups val)
    PdxArray xs -> PdxArray (map genBuildingGroups xs)
    other -> other

genLaws :: PdxValue -> PdxValue
genLaws v = case v of
    PdxPair (key, op, PdxNumber n)
        | key `elem` ["tax_per_capita_add", "tax_land_add"] -> PdxPair (key, op, PdxNumber $ n * nominalFactor - n)
    PdxPair (key, op, val) -> PdxPair (key, op, genLaws val)
    PdxArray xs -> PdxArray (map genLaws xs)
    other -> other

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
        , ("BUY_SELL_DIFF_AT_MAX_FACTOR", const $ PdxNumber maxDiffFactor)
        , ("GOODS_SHORTAGE_PENALTY_THRESHOLD", const $ PdxNumber $ 1 / maxDiffFactor)
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

tradeValues = PdxArray [PdxPair ("used_trade_capacity", "=", PdxArray [
        PdxPair ("value", "=", PdxUString "state_trade"), 
        PdxPair ("divide", "=", PdxNumber $ basePrice / baseTradeCap * nominalFactor)
        ])
    ]

Just (_,cbpValues) = runParser pdxValue $ unlines [
    "{",
    "bem_state_construction_sell_orders = {",
    "    sg:construction = {value = state_goods_production}",
    "}",
    "bem_state_capex = {",
    "    value = owner.investment_pool_income",
    "    multiply = bem_state_urbanization_share",
    "}",
    "bem_cbp_b_lower = {",
    "    value = bem_state_capex",
    "    divide = " ++ show (basePrice - (basePrice * priceRange)), 
    "}",
    "bem_cbp_lower_bound = {",
    "    value = bem_state_construction_sell_orders",
    "    divide = " ++ show maxDiffFactor,
    "}",
    "bem_cbp_b_upper = {",
    "    value = bem_state_capex",
    "    divide = " ++ show (basePrice + (basePrice * priceRange)),
    "}",
    "bem_cbp_upper_bound = {",
    "    value = bem_state_construction_sell_orders",
    "    multiply = " ++ show maxDiffFactor,
    "}",
    "bem_cbp_ndelta_a = {",
    "    value = " ++ show (basePrice * (1 + priceRange / (maxDiffFactor - 1))),
    "}",
    "bem_cbp_ndelta_b = {",
    "    value = " ++ show (-basePrice),
    "    multiply = {",
    "        value = " ++ show priceRange,
    "        multiply = bem_state_construction_sell_orders",
    "        divide = " ++ show (maxDiffFactor - 1),
    "    }",
    "    subtract = bem_state_capex",
    "}",
    "bem_cbp_ndelta = {",
    "    value = 0",
    "    subtract = bem_cbp_ndelta_b",
    "    divide = bem_cbp_ndelta_a",
    "}",
    "bem_cbp_pdelta_a = {",
    "    value = " ++ show basePrice,
    "    multiply = {",
    "        value = " ++ show priceRange,
    "        divide = {",
    "            value = " ++ show (maxDiffFactor - 1),
    "            multiply = bem_state_construction_sell_orders",
    "        }",
    "    }",
    "}",
    "bem_cbp_pdelta_b = {",
    "    value = " ++ show (basePrice * (1 - priceRange / (maxDiffFactor - 1))),
    "}",
    "bem_cbp_pdelta_c = {",
    "    value = 0",
    "    subtract = bem_state_capex",
    "}",
    "bem_cbp_pdelta = {",
    "    value = 0",
    "    subtract = bem_cbp_pdelta_b",
    "    add = {",
    "        value = bem_cbp_pdelta_b",
    "        multiply = bem_cbp_pdelta_b",
    "        subtract = {",
    "            value = 4",
    "            multiply = bem_cbp_pdelta_a",
    "            multiply = bem_cbp_pdelta_c",
    "        }",
    "        pow = 0.5",
    "    }",
    "    divide = {",
    "        value = 2",
    "        multiply = bem_cbp_pdelta_a",
    "    }",
    "}",
    "bem_construction_buy_package = {",
    "    if = {",
    "        limit = {bem_cbp_b_upper >= bem_cbp_upper_bound}",
    "        value = bem_cbp_b_upper",
    "    }",
    "    else_if = {",
    "        limit = {bem_cbp_b_lower <= bem_cbp_lower_bound}",
    "        value = bem_cbp_b_lower",
    "    }",
    "    else_if = {",
    "        limit = {bem_cbp_ndelta <= bem_state_construction_sell_orders}",
    "        value = bem_cbp_ndelta",
    "    }",
    "    else = {",
    "        value = bem_cbp_pdelta",
    "    }",
    "}",
    "}"
    ] 

polstrength :: Double -> Double
polstrength wealth
    | wealth <= 10 = wealth * 0.03
    | wealth <= 25 = (wealth - 10) ** 1 * 0.48 + 0.3
    | wealth <= 45 = (wealth - 15) ** 1.5 + 8
    | otherwise = (wealth - 45) ** 2 * 2 + 70

totalExpenditures :: Double -> Double
totalExpenditures wealth = (1.095 ** (50 + 1 * wealth) + 75) * nominalFactor

weightBasicFood :: Double -> Double
weightBasicFood wealth
    | wealth > 0 && wealth <= 18 = 5 * wealth + 90
    | wealth > 18 && wealth <= 25 = 180
    | wealth > 24 && wealth <= 29 = -5 * wealth + 305
    | otherwise = 0

weightCrudeItems :: Double -> Double
weightCrudeItems wealth
    | wealth > 4 && wealth <= 14 = -1.5 * ((wealth - 9) ** 2) + 50
    | otherwise = 0

weightSimpleClothing :: Double -> Double
weightSimpleClothing wealth
    | wealth > 0 && wealth <= 14 = -0.33 * ((wealth - 8) ** 2) + 50
    | otherwise = 0

popneedHeating :: Double -> Double
popneedHeating wealth
    | wealth > 0 && wealth < 9 = wealth + 17
    | wealth >= 9 = 26
    | otherwise = 0

weightHouseholdItems :: Double -> Double
weightHouseholdItems wealth
    | wealth > 10 && wealth < 30 = 12 * wealth - 113
    | wealth >= 30 && wealth < 45 = 247
    | otherwise = 0

weightStandardClothing :: Double -> Double
weightStandardClothing wealth
    | wealth > 10 && wealth < 24 = 12 * wealth - 113
    | wealth >= 24 && wealth < 39 = 175
    | otherwise = 0

weightServices :: Double -> Double
weightServices wealth
    | wealth > 6 = 8 * wealth - 50
    | otherwise = 0

popneedIntoxicants :: Double -> Double
popneedIntoxicants wealth
    | wealth > 0 && wealth < 50 = 4 * wealth
    | wealth >= 50 = 4 * 50
    | otherwise = 0

weightLuxuryDrinks :: Double -> Double
weightLuxuryDrinks wealth
    | wealth >= 15 = (1.145 ** wealth) + 20
    | otherwise = 0

weightFreeMovement :: Double -> Double
weightFreeMovement wealth
    | wealth >= 10 = 1.145 ** wealth
    | otherwise = 0

weightCommunication :: Double -> Double
weightCommunication wealth
    | wealth >= 20 = (1.145 ** wealth) + 20
    | otherwise = 0

weightLuxuryFood :: Double -> Double
weightLuxuryFood wealth
    | wealth >= 20 = (1.145 ** wealth) + 20
    | otherwise = 0

weightLuxuryItems :: Double -> Double
weightLuxuryItems wealth
    | wealth >= 15 = (1.165 ** wealth) + 20
    | otherwise = 0

weightLeisure :: Double -> Double
weightLeisure wealth
    | wealth >= 20 = (1.165 ** wealth) + 20
    | otherwise = 0

weightFinancialServices :: Double -> Double
weightFinancialServices wealth
    | wealth >= 30 = (1.3 ** (wealth - 30)) + 50
    | otherwise = 0

weightSum :: Double -> Double
weightSum wealth = sum
    [ weightBasicFood wealth
    , weightCrudeItems wealth
    , weightSimpleClothing wealth
    , weightHouseholdItems wealth
    , weightStandardClothing wealth
    , weightServices wealth
    , weightLuxuryDrinks wealth
    , weightFreeMovement wealth
    , weightCommunication wealth
    , weightLuxuryFood wealth
    , weightLuxuryItems wealth
    , weightLeisure wealth
    , weightFinancialServices wealth
    ]

realExpenditures :: Double -> Double -> Double
realExpenditures weight wealth = 
    fromIntegral $ round $ (weight / weightSum wealth) * totalExpenditures wealth

buyPackage :: Double -> Double -> String
buyPackage weight wealth = 
    let real = realExpenditures weight wealth
        total = totalExpenditures wealth
        percentage = (real / total) * 100
    in printf "%.0f # %.2f%%" real percentage

buyPackageValue :: Double -> Double -> PdxValue
buyPackageValue weight wealth = 
    let real = realExpenditures weight wealth
        total = totalExpenditures wealth
        percentage = (real / total) * 100
        comment = printf "%.2f%%" percentage :: String
    in PdxUString $ printf "%.0f # %s" real comment

unweightedValue :: Double -> PdxValue
unweightedValue val = PdxUString $ printf "%.0f # unweighted" val

goods :: Double -> PdxValue
goods wealth = PdxArray
    [ PdxPair ("popneed_basic_food", "=", buyPackageValue (weightBasicFood wealth) wealth)
    , PdxPair ("popneed_crude_items", "=", buyPackageValue (weightCrudeItems wealth) wealth)
    , PdxPair ("popneed_simple_clothing", "=", buyPackageValue (weightSimpleClothing wealth) wealth)
    , PdxPair ("popneed_heating", "=", unweightedValue (popneedHeating wealth))
    , PdxPair ("popneed_household_items", "=", buyPackageValue (weightHouseholdItems wealth) wealth)
    , PdxPair ("popneed_standard_clothing", "=", buyPackageValue (weightStandardClothing wealth) wealth)
    , PdxPair ("popneed_services", "=", buyPackageValue (weightServices wealth) wealth)
    , PdxPair ("popneed_intoxicants", "=", unweightedValue (popneedIntoxicants wealth))
    , PdxPair ("popneed_luxury_drinks", "=", buyPackageValue (weightLuxuryDrinks wealth) wealth)
    , PdxPair ("popneed_free_movement", "=", buyPackageValue (weightFreeMovement wealth) wealth)
    , PdxPair ("popneed_communication", "=", buyPackageValue (weightCommunication wealth) wealth)
    , PdxPair ("popneed_luxury_food", "=", buyPackageValue (weightLuxuryFood wealth) wealth)
    , PdxPair ("popneed_luxury_items", "=", buyPackageValue (weightLuxuryItems wealth) wealth)
    , PdxPair ("popneed_leisure", "=", buyPackageValue (weightLeisure wealth) wealth)
    , PdxPair ("popneed_financial_services", "=", buyPackageValue (weightFinancialServices wealth) wealth)
    ]

wealthEntry :: Int -> PdxValue
wealthEntry i = 
    let wealth = fromIntegral i
        polStr = printf "%.3f" (polstrength wealth) :: String
    in PdxPair ("wealth_" ++ show i, "=", PdxArray
        [ PdxPair ("political_strength", "=", PdxNumber (polstrength wealth))
        , PdxPair ("goods", "=", goods wealth)
        ])

genBuyPackages :: Double -> PdxValue
genBuyPackages n = PdxArray $ map wealthEntry [1..floor n]

genGoodInputsGeneric :: [(String, Double)] -> PdxValue -> PdxValue
genGoodInputsGeneric costMap v = case v of
    PdxPair (key, op, PdxNumber n) -> case snd <$> runParser inputP key of
        Just goodName -> case lookup goodName costMap of
                Just oBasePrice -> PdxPair (key, op, PdxNumber $ oBasePrice / basePrice * n * nominalFactor)
                Nothing -> PdxPair (key, op, PdxNumber n)
        Nothing -> PdxPair (key, op, PdxNumber n)
    PdxPair (key, op, val) -> PdxPair (key, op, genGoodInputsGeneric costMap val)
    PdxArray xs ->  PdxArray (map (genGoodInputsGeneric costMap) xs)
    other -> other
    where
        inputP = (\_ key -> key) 
            <$> stringP "goods_input_" <*> spanUntilP "_add"

main :: IO ()
main = do 
    bemCommon <- (</> "better-economy-mod/common") . init <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
    vic3Common <- (</> ".local/share/Steam/steamapps/common/Victoria 3/game/common") <$> getHomeDirectory
    Just goods <- parseFile (vic3Common </> "goods/00_goods.txt")
    let costMap = baseCosts goods
    processDirectory Replace ["00", "05", "08"] (genPMs costMap) "production_methods"
    processDirectory Replace [] genGoods "goods"
    processDirectory Replace [] genPoptypes "pop_types"
    processDirectory None ["99"] genStateRegions "../map_data/state_regions"
    processDirectory None [] genHistory "history/buildings"
    processDirectory Replace [] genBuildingGroups "building_groups"
    processDirectory Replace [] genLaws "laws"
    processDirectory None [] genDefines "defines"
    processDirectory ReplaceOrCreate [] (const $ genBuyPackages wealthLevels) "buy_packages"
    processDirectory Replace [] genBuildings "buildings"
    processDirectory Replace [] (genGoodInputsGeneric costMap) "combat_unit_types"
    processDirectory Replace [] (genGoodInputsGeneric costMap) "mobilization_options"
    writeFile (bemCommon </> "script_values/bem_trade_values.txt") $ toPdxScript tradeValues
    writeFile (bemCommon </> "script_values/bem_investment_values.txt") $ toPdxScript cbpValues