module Lifegame
    ( game
    ) where

-- http://d.hatena.ne.jp/lamuu/20060825/1156520199

import System.Random (getStdRandom, randomR) 
import Data.Array.IO (newArray, IOUArray, writeArray, readArray)


randInt :: IO Int
randInt = getStdRandom (randomR (1,10))


randField :: Int -> Int -> IOUArray (Int, Int) Bool -> IO ()
randField x y field = randInt >>= (\r -> writeArray field (x,y) (r == 1))


around = [
  (-1,-1),(0,-1),(1,-1),
  (-1, 0),       (1, 0),
  (-1, 1),(0, 1),(1, 1)]

glider = [False, True, False, False, True, True, True, True]
createGlider :: Int -> Int -> IOUArray (Int, Int) Bool -> IO [()]
createGlider x0 y0 field = do
    writeArray field (x0,y0) False
    mapM (\((x,y),b) -> writeArray field (x0+x,y0+y) b) $ zip around glider


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


game :: IO ()
game = do
    field1 <- newArray ((0,0),(99,99)) False :: IO (IOUArray (Int, Int) Bool)
    field2 <- newArray ((0,0),(99,99)) False :: IO (IOUArray (Int, Int) Bool)
    mapM_ (\(x,y) -> randField x y field1) points
    mapM_ (\(x,y) -> createGlider x y field1) [(10,10),(20,20),(30,30)]
    n <- randInt
    putStrLn $ show n 
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
    return ()
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
