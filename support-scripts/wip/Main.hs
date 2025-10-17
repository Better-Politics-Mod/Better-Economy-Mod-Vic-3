module Main where

import PdxParser
import System.Environment (getArgs)

main :: IO ()
main = getArgs
    >>= parseFile . head <*> pure pdxValue
    >>= print