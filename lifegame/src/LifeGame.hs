{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LifeGame
    ( LifeGameIO
    , game
    ) where

-- reference
-- http://d.hatena.ne.jp/lamuu/20060825/1156520199
-- http://d.hatena.ne.jp/nskj77/20070929/1191074772

import Control.Monad (join)
import Control.Monad.Trans (MonadIO(..))
import Data.Array.IO (newArray, IOUArray, writeArray, readArray, newListArray, getBounds)
import Data.Array.Unboxed (UArray, array, listArray, bounds, (!))
import System.Random (getStdRandom, randomR)

--      + Type Constructor
--      |          + Type Parameter
--      |          |   + Data Constructor: LifeGameIO :: IO a -> LifeGameIO a
--      |          |   |          + Data Parameter (Record Statement)
--      |          |   |          |
newtype LifeGameIO a = LifeGameIO {
--    + Accessor
--    |                + Type
--    |                |
      runLifeGameIO :: IO a
--    + Type Class auto deriving to instance method
--    |        + Super Classes (or deriving (Applicative, Monad) )
--    |        |
    } deriving (Functor, Applicative, Monad)

-- + LifeGameIO is instance of MonadIO Type Class
-- |
instance MonadIO LifeGameIO where
--  + ideal method: liftIO :: IO a -> m a
--  |        + actual method: LifeGameIO :: IO a -> LifeGameIO a
--  |        |
    liftIO = LifeGameIO

--   + Type Constructor
--   |      
--   |       + Mutable, unboxed, strict arrays
--   |       |        + width and height Index Types
--   |       |        |          + element type
--   |       |        |          |
type Field = IOUArray (Int, Int) Bool
{--
instance Show Field where
    show field = join $ do
--                                   + getBounds :: (MArray a e m, Ix i) => a i e -> m (i, i)
--                                   |
        ((_, _), (width, height)) <- liftIO $ getBounds field :: IO ((Int, Int), (Int, Int))
        return $ show (width, height)
--}
newField :: Int -> Int -> LifeGameIO Field
--                      + LifeGameIO :: IO Field -> LifeGameIO Field
--                      |            + newArray :: Ix i => (i, i) -> e -> m (a i e)
--                      |            |
newField width height = LifeGameIO $ newArray ((0, 0), (width, height)) False

--             + Arrays with unboxed elements. Instances of
--             |      + index type
--             |      |   + element type
--             |      |   |
type Pattern = UArray (Int, Int) Bool

--        + UArray Int Bool
--        |
glider :: Pattern
--       + listArray :: (Int, Int) -> [Bool] -> UArray Int Bool
--       |
--       |          + glider!(0, 0) == False
--       |          |       + glider!(2, 2) == True
--       |          |       |
glider = listArray ((0, 0), (2, 2)) [ False, True,  False
                                    , False, False, True 
                                    , True,  True,  True ]

--            + IOUArray (Int, Int) Bool
--            |                      + UArray Int Bool
--            |                      |
setPattern :: Field -> Int -> Int -> Pattern -> LifeGameIO ()
setPattern field offsetX offsetY pattern = do
    let ((_, _), (width, height)) = bounds pattern
    let offsets = [(x, y) | x <- [1..width], y <- [1..height]] :: [(Int, Int)]
--  mapM_ :: ((Int, Int) -> IO ()) -> [(Int, Int)] -> IO ()
--  |
    mapM_ put offsets
    where
--                            + writeArray :: IOUArray (Int, Int) Bool -> (Int, Int) -> Bool -> IO ()
--                            |          + IOUArray (Int, Int) Bool
--                            |          |                                + UArray (Int, Int) Bool
--                            |          |                                |
        put (x, y) = liftIO $ writeArray field (offsetX + x, offsetY + y) $ pattern!(x, y)




game :: IO ()
--     + runLifeGameIO :: IO ()
--     |
game = runLifeGameIO $ do
--    + liftIO :: IO a -> LifeGameIO a
--    |        + putStrLn :: String -> IO ()
--    |        |
      liftIO $ putStrLn $ show glider
--              + newField :: Int -> Int -> LifeGameIO Field
--              |
      field1 <- newField 100 100
      field2 <- newField 100 100
--    + setPattern :: Field -> Int -> Int -> Pattern -> LifeGameIO ()
--    |
      setPattern field1 0 0 glider
      liftIO $ putStrLn $ show field1
      return ()
--    mapM_ (\(x,y) -> randField x y field1) points
--    mapM_ (\(x,y) -> createGlider x y field1) [(10,10),(20,20),(30,30)]
    {-
    frame <- frameCreate objectNull idAny "Lifegame" rectNull
        (wxSYSTEM_MENU + wxCAPTION + wxNO_FULL_REPAINT_ON_RESIZE)
    windowSetClientSize frame (sz 100 100)
    panel <- panelCreate frame idAny rectNull 0
    timer <- windowTimerCreate frame
    timerOnCommand timer (nextGen field1 field2 panel timer)
    windowOnPaintRaw panel (drawCells field1)
    windowShow frame
    timerStart timer 500 False
    
    where
        --タイマーに登録する世代交代処理
        nextGen now next w t
        = do mapM_ (\(x,y) -> nextField x y now next)
    　　　　　　　　　　　　　　　　　　　　　　　　　[(x,y)|x <- [1..98],y <- [1..98]]
                timerOnCommand t (nextGen next now w t) --世代をスワップして再登録
                windowRefresh w True

        --全てのセルを描画する
        drawCells field dc viewRect updateAreas
        = do mapM_ (\(x,y) ->
                do alive <- readArray field (x,y)
                    when alive (drawPoint dc (pt x y) [color := black])) points
-}


{-
neighbourhood :: [(Int, Int)]
neighbourhood = [ (-1,-1), (0,-1), (1,-1)
                , (-1, 0), (0, 0), (1, 0)
                , (-1, 1), (0, 1), (1, 1) ]

nextField :: Int ->Int ->IOUArray (Int,Int) Bool ->IOUArray (Int,Int) Bool ->IO()
nextField x0 y0 now next = do
    alive <- readArray now (x0,y0)
    aroundAlive <- mapM (\(x,y) -> readArray now (x0+x,y0+y)) around
    case (count aroundAlive) of
        3 -> writeArray next (x0,y0) True
        2 -> writeArray next (x0,y0) alive
        _ -> writeArray next (x0,y0) False
    where
        count [] = 0
        count (x:xs) = if x then 1 + count xs else count xs

points = [(x, y)| x <- [0..99], y <- [0..99]]
-}

{-
--                               + IOUArray (Int, Int) Bool
--                               |
randFieldOneCell :: Int -> Int -> Field -> LifeGameIO ()
randFieldOneCell x y field = do
--       + randInt :: LifeGameIO Int
--       |
    r <- randRange (1, 10)
    let flag = r == 1 -- 1/10 probabry
--  + liftIO :: IO () -> LifeGameIO ()
--  |        + writeArray :: IOUArray (Int, Int) Bool -> (Int, Int) -> Bool -> IO ()
--  |        |          + IOUArray (Int, Int) Bool
--  |        |          |            + Bool
--  |        |          |            |
    liftIO $ writeArray field (x, y) flag
-}

{-
randRange :: (Int, Int) -> LifeGameIO Int
--                + LifeGameIO :: IO Int -> LifeGameIO Int
--                |            + getStdRandom :: (StdGen -> (Int, StdGen)) -> IO Int
--                |            |              + randomR :: (Int, Int) -> StdGen -> (Int, StdGen)
--                |            |              |
--                |            |              |
randRange range = LifeGameIO $ getStdRandom $ randomR range
-}