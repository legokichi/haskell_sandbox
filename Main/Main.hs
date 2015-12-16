module Main (main) where

import SimpleJSON

main :: IO ()
main = print (JObject [("foo", JNumber 1), ("bar", JNumber 2)])
