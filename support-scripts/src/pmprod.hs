module Main where

import PdxParser
import Data.Maybe
import Text.Printf

printOutputInputRatios :: PdxValue -> IO ()
printOutputInputRatios (PdxArray xs) = mapM_ printRatio xs
  where
    printRatio (PdxPair (key, _, val)) = 
        let (out, inp) = collect val
            ratio = if inp > 0 then out / inp else 0
        in printf "%s: %.3f\n" key ratio
    printRatio _ = pure ()
    
    collect v = let (outs, ins) = go v in (sum outs, sum ins)
    
    go (PdxArray xs) = mconcat $ map go xs
    go (PdxPair (k, _, PdxNumber n)) 
        | isJust (snd <$> runParser outputP k) = ([n], [])
        | isJust (snd <$> runParser inputP k) = ([], [n])
    go (PdxPair (_, _, v)) = go v
    go _ = ([], [])
    
    outputP = (\_ _ key -> key) <$> stringP "goods_" <*> stringP "output_" <*> spanUntilP "_add"
    inputP = (\_ _ key -> key) <$> stringP "goods_" <*> stringP "input_" <*> spanUntilP "_add"
printOutputInputRatios _ = pure ()

main :: IO ()
main = do 
    Just file <- parseFile "/persist/home/.local/share/Steam/steamapps/common/Victoria 3/game/common/production_methods/01_industry.txt"
    printOutputInputRatios $ file