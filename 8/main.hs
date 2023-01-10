import Data.Char (digitToInt)

indexGrid :: [[Int]] -> Int -> Int -> Int
indexGrid grid x y = (grid !! y) !! x 

parseLine :: [String] -> [Int]
parseLine line = map (\x -> (read (x:"") :: Int)) line
