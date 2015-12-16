{-|
  Real World Haskell chaptor 5
-}

module SimpleJSON
  ( JValue(..)
  , getString
  , getInt
  , getDouble
  , getBool
  , getObject
  , getArray
  , isNull
  ) where

-- import Prelude

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

-- http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:truncate
-- truncate x returns the integer nearest x between zero and x

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull v = v == JNull
