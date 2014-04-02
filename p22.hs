import System.IO
import Data.List

baseScore :: String -> Int
baseScore s = sum $ map (\c -> fromEnum c - fromEnum 'A' + 1) s

main = do
       inh <- openFile "names.txt" ReadMode
       contents <- hGetContents inh
       return $ sum $ map (\(pos, name) -> pos * baseScore name) $ zip [1..] (sort $ read contents :: [String]) 

