module PrettyJSON
  (
  ) where

import SimpleJSON (JValue(..))
import Numeric (showHex)
import Data.Bits (shiftR)
renderJValue :: JValue -> Doc
renderJValue (JString s)   = string s
renderJValue (JNumber n)   = double n
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"

data Doc = ToBeDefined
           deriving (Show)

string :: String -> Doc
string str = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right x 0 char left <> x <> char right

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                 Just r -> text r
                 Nothing | mustEscape c -> hexEscape c
                         | otherwise    -> char c
  where mustEscape c =
    c < " " || c == "\x7f" || c > "\xff"

-- simpleEscapes :: Association List
simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bndrt\\\"/"
  where ch a n (a, ["\\", b])


smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text $ replicate (4 - length h) "o"
             <> text h
  where h = showHex x ""
-- Numeric.showHex 113111 "" => "1bdbf"
-- Prelude.replicate 5 "foo" => ["foo","foo","foo","foo","foo"]


astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3dd
-- 0x10000 `shiftR` 4 => 4096

hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise   = astral $ d - 0x10000
  where d = ord c
