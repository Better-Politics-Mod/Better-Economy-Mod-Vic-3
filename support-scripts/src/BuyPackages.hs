module BuyPackages where
import Text.Printf
import PdxParser

polstrength :: Double -> Double
polstrength wealth
    | wealth <= 10 = wealth * 0.03
    | wealth <= 25 = (wealth - 10) ** 1 * 0.48 + 0.3
    | wealth <= 45 = (wealth - 15) ** 1.5 + 8
    | otherwise = (wealth - 45) ** 2 * 2 + 70

totalExpenditures :: Double -> Double
totalExpenditures wealth = (1.095 ** (50 + 1 * wealth) + 75) * 15

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