{-# LANGUAGE LambdaCase #-}
module Main where

import PdxParser
import Data.Maybe
import Data.List
import Data.Function 
import Data.Ord 
import Text.Printf

findPairValue :: String -> [PdxValue] -> Maybe (Either String Int)
findPairValue key = listToMaybe . mapMaybe match
  where
    match (PdxPair (k, _, v)) | k == key = case v of
        PdxQString s -> Just $ Left s
        PdxNumber n -> Just $ Right $ floor n
        _ -> Nothing
    match _ = Nothing

extractDatabase :: String -> (PdxValue -> Maybe a) -> PdxValue -> [a]
extractDatabase manager process (PdxArray xs) = concatMap (extractDatabase manager process) xs
extractDatabase manager process (PdxPair (key, _, PdxArray items)) 
    | key == manager = maybe [] id $ do
        PdxPair ("database", _, PdxArray entries) <- find isDatabase items
        pure $ mapMaybe process entries
  where isDatabase (PdxPair ("database", _, _)) = True
        isDatabase _ = False
extractDatabase _ _ _ = []

countryTags :: PdxValue -> [(Int, String)]
countryTags = extractDatabase "country_manager" $ \case
    PdxPair (key, _, PdxArray fields) -> (,) 
        <$> (Just $ read key) 
        <*> (findPairValue "definition" fields >>= either Just (const Nothing))
    _ -> Nothing

stateTags :: PdxValue -> [(Int, Int)]
stateTags = extractDatabase "states" $ \case
    PdxPair (key, _, PdxArray fields) -> (,) 
        <$> (findPairValue "country" fields >>= either (const Nothing) Just) 
        <*> Just (read key)
    _ -> Nothing

extractPopData :: PdxValue -> [(Int, Double, Int)]
extractPopData = extractDatabase "pops" $ \case
    PdxPair (_, _, PdxArray fields) -> (,,) 
        <$> findInt "location" fields 
        <*> Just (sumBudget fields) 
        <*> Just (sumPop fields)
    _ -> Nothing
  where
    findInt key fields = floor <$> findNum key fields
    findNum key fields = listToMaybe [n | PdxPair (k, _, PdxNumber n) <- fields, k == key]
    
    sumBudget fields = case find isBudget fields of
        Just (PdxPair (_, _, PdxArray nums)) -> 
            sum [n | (i, PdxNumber n) <- zip [0..] nums, i /= 7]
        _ -> 0
      where
        isBudget (PdxPair ("weekly_budget", _, _)) = True
        isBudget _ = False
    
    sumPop fields = sum $ mapMaybe (`findInt` fields) ["workforce", "dependents"]

groupStatesByTag :: [(Int, String)] -> [(Int, Int)] -> [(String, [Int])]
groupStatesByTag idToTag idToStates = 
    map (\grp -> (fst $ grp !! 0, map snd grp)) $
    groupBy ((==) `on` fst) $
    sortBy (compare `on` fst) $
    [(tag, stateId) | (countryId, stateId) <- idToStates, 
                      Just tag <- [lookup countryId idToTag]]

groupPopsByTag :: [(String, [Int])] -> [(Int, Double, Int)] -> [(String, [(Double, Int)])]
groupPopsByTag tagLocations popData = 
    [(tag, [(budget, pop) | (loc, budget, pop) <- popData, loc `elem` locs]) 
     | (tag, locs) <- tagLocations]
    & filter (not . null . snd)

giniCoefficient :: [(Double, Int)] -> Double
giniCoefficient pairs
  | null pairs || totalPop <= 0 || totalIncome <= 0 = 0
  | otherwise = 1 - 2 * lorenzArea
  where
    sorted = sortBy (comparing fst) [(i, fromIntegral p) | (i, p) <- pairs]
    totalPop = sum $ map snd sorted
    totalIncome = sum [i * p | (i, p) <- sorted]
    cumulative = scanl (\(cp, ci) (i, p) -> (cp + p, ci + i * p)) (0, 0) sorted
    lorenzArea = sum [trapezoid p1 p2 | (p1, p2) <- zip cumulative (drop 1 cumulative)]
    trapezoid (cp1, ci1) (cp2, ci2) = 
        let (x1, x2) = (cp1 / totalPop, cp2 / totalPop)
            (y1, y2) = (ci1 / totalIncome, ci2 / totalIncome)
        in (x2 - x1) * (y1 + y2) / 2

main :: IO ()
main = do
    Just save <- parseFile "/persist/home/.local/share/Paradox Interactive/Victoria 3/save games/great britain_1836_01_01.v3"
    let tags = countryTags save
        states = stateTags save
        statesByTag = groupStatesByTag tags states
        popsByTag = groupPopsByTag statesByTag (extractPopData save)
        giniByCountry = map (fmap giniCoefficient) popsByTag
        results = sortBy (comparing snd) giniByCountry
    writeFile "./out2.txt" $ unlines [tag ++ ": " ++ printf "%.3f" gini | (tag, gini) <- results]