module Main where

import Lib
import Data.String.Utils (join)


main :: IO ()
main = do
    putStrLn $ join "\n" $ fun `map` [0..1000]
