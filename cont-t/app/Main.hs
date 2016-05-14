module Main where

import qualified Lib
import qualified Lib2
import Data.String.Utils (join)


main :: IO ()
main = do
    putStrLn $ join "\n" $ Lib.fun `map` [0..10]
    a <- Lib2.fun
    putStrLn a
