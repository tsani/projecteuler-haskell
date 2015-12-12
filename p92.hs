module Main where

import Control.Monad.State.Strict
import Data.Digits
import Data.IORef
import qualified Data.IntMap.Strict as M

step :: Int -> Int
step = sum . map (^2) . digits 10

goesTo89 :: Int -> State (M.IntMap Bool) Bool
goesTo89 1 = pure False
goesTo89 89 = pure True
goesTo89 n = do
    m <- gets (M.lookup n)
    case m of
        Just t -> pure t
        Nothing -> do
            t <- goesTo89 (step n)
            modify (M.insert n t)
            pure t

goesTo89IO :: IORef (M.IntMap Bool) -> Int -> IO Bool
goesTo89IO _ 1 = pure False
goesTo89IO _ 89 = pure True
goesTo89IO r n = do
    m <- readIORef r
    case M.lookup n m of
        Just t -> pure t
        Nothing -> do
            t <- goesTo89IO r (step n)
            writeIORef r $ M.insert n t m
            pure t

answerPure = length $ flip evalState M.empty $ filterM goesTo89 [1..10000000]

main = do
    r <- newIORef M.empty
    answer <- length <$> filterM (goesTo89IO r) [1..10000000]
    print answer
