import Prelude
import Control.Monad.State
import qualified Data.Map.Strict as M

type Point = (Int, Int)
type Path = [Point]

makeBasicPaths' :: Point -> State (M.Map Point [Path]) [Path]
makeBasicPaths' (x, y) = do 
                         table <- get
                         case M.lookup (x, y) table of
                             Just ps -> return ps
                             Nothing -> do
                                        ps1 <- if x + 1 < 20
                                               then makeBasicPaths' (x + 1, y)
                                               else return []
                                        ps2 <- if y + 1 < 20 
                                               then makeBasicPaths' (x, y + 1)
                                               else return []
                                        put $ M.insert (x, y) (map ((:) (x, y)) (ps1 ++ ps2)) table
                                        return $ map ((:) (x, y)) (ps1 ++ ps2)

makeBasicPaths :: Point -> State (M.Map Point [Path]) [Path]
makeBasicPaths (x, y)
    | x + 1 == 21 && y + 1 == 21 = return [[(x, y)]]
    | x + 1 == 21 || y + 1 == 21 = do
                                   table <- get
                                   case M.lookup (x, y) table of
                                       Nothing -> do
                                                  ps <- let pt = if x + 1 == 21 then (x, y + 1) else (x + 1, y)
                                                            in makeBasicPaths pt
                                                  put $ M.insert (x, y) (map ((:) (x, y)) ps) table
                                                  return $ map ((:) (x, y)) ps
                                       Just ps -> return ps
    | otherwise                  = do
                                   table <- get
                                   case M.lookup (x, y) table of
                                       Nothing -> do
                                                  ps1 <- makeBasicPaths (x + 1, y)
                                                  ps2 <- makeBasicPaths (x, y + 1)
                                                  put $ M.insert (x, y) (map ((:) (x, y)) (ps1 ++ ps2)) table
                                                  return $ map ((:) (x, y)) (ps1 ++ ps2)
                                       Just ps -> return ps

main = print . length $ evalState (makeBasicPaths (0, 0)) (M.fromList [])



