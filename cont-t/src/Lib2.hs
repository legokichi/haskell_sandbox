
module Lib2
    ( fun
    ) where

import Prelude
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Cont (runContT, callCC)
import Data.Char (digitToInt, intToDigit)

-- http://www.sampou.org/haskell/a-a-monads/html/transformers.html

fun :: IO String
fun = (`runContT` return) $ do
        n   <- liftIO (readLn::IO Int)
        str <- callCC $ \exit1 -> do
          when (n < 10) (exit1 (show n))
          let ns = map digitToInt (show (n `div` 2))
          n' <- callCC $ \exit2 -> do
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) $ do
              liftIO $ putStrLn "Enter a number:"
              x <- liftIO (readLn::IO Int)
              exit2 x
            when ((length ns) < 7) $ do
              let ns' = map intToDigit (reverse ns)
              exit1 (dropWhile (=='0') ns')
            return $ sum ns
          return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
        return $ "Answer: " ++ str