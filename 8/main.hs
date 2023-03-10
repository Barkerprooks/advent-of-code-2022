module Main where

import Text.Printf (printf)


-- grid utilities
indexGrid :: [[Integer]] -> Int -> Int -> Integer
indexGrid grid x y = (grid !! y) !! x

iterF :: [Integer] -> Int -> [Integer]
iterF xs i = drop (i + 1) xs

iterB :: [Integer] -> Int -> [Integer]
iterB xs i = reverse (take i xs)

gridCol :: [[Integer]] -> Int -> [Integer]
gridCol grid x = [indexGrid grid x y | y <- [0..l]]
  where l = (length grid) - 1


-- part 1
check :: [Integer] -> Integer -> Bool
check [] _ = True
check (x:xs) v = if v <= x then False else check xs v

checkN :: [[Integer]] -> Int -> Int -> Bool
checkN grid x y = check (iterB (gridCol grid x) y) (indexGrid grid x y)

checkS :: [[Integer]] -> Int -> Int -> Bool
checkS grid x y = check (iterF (gridCol grid x) y) (indexGrid grid x y)

checkE :: [[Integer]] -> Int -> Int -> Bool
checkE grid x y = check (iterB (grid !! y) x) (indexGrid grid x y)

checkW :: [[Integer]] -> Int -> Int -> Bool
checkW grid x y = check (iterF (grid !! y) x) (indexGrid grid x y)

checkCell :: [[Integer]] -> Int -> Int -> Int
checkCell grid x y = fromEnum (or [checkN grid x y, checkS grid x y, checkE grid x y, checkW grid x y])

checkGridRow :: [[Integer]] -> Int -> [Int]
checkGridRow grid y = [checkCell grid x y | x <- [0..n]]
  where n = (length (grid !! y)) - 1

checkGrid :: [[Integer]] -> [[Int]]
checkGrid grid = [checkGridRow grid y | y <- [0..((length grid) - 1)]]


-- part 2
countVisible :: [Integer] -> Integer -> Integer
countVisible [] _ = 0
countVisible (x:xs) v
  | x >= v = 1
  | otherwise = 1 + countVisible xs v

countN :: [[Integer]] -> Int -> Int -> Integer
countN grid x y = countVisible (iterB (gridCol grid x) y) (indexGrid grid x y)

countS :: [[Integer]] -> Int -> Int -> Integer
countS grid x y = countVisible (iterF (gridCol grid x) y) (indexGrid grid x y)

countE :: [[Integer]] -> Int -> Int -> Integer
countE grid x y = countVisible (iterB (grid !! y) x) (indexGrid grid x y)

countW :: [[Integer]] -> Int -> Int -> Integer
countW grid x y = countVisible (iterF (grid !! y) x) (indexGrid grid x y)

countCell :: [[Integer]] -> Int -> Int -> Integer
countCell grid x y = (countN grid x y) * (countS grid x y) * (countE grid x y) * (countW grid x y)

countGridRow :: [[Integer]] -> Int -> Integer
countGridRow grid y = maximum [countCell grid x y | x <- [0..l]]
  where l = (length grid) - 1

countGrid :: [[Integer]] -> Integer
countGrid grid = maximum [countGridRow grid y | y <- [0..((length grid) - 1)]]


-- file reading utilities
parseLine :: String -> [Integer]
parseLine l = fmap (\c -> read (c:"") ::Integer) l

parseText :: String -> [[Integer]]
parseText text = fmap (\l -> parseLine l) (lines text)


--
main :: IO ()
main = do

  text <- readFile "input.txt"

  let grid = parseText text

  printf "part 1: %d\n" (sum (fmap (sum) (checkGrid grid)))
  printf "part 2: %d\n" (countGrid grid)
