grid = [[1, 2, 3, 4, 5],
        [2, 3, 3, 3, 4],
        [5, 5, 6, 4, 5],
        [2, 3, 1, 2, 4],
        [6, 4, 5, 6, 4]]

indexGrid :: [[Integer]] -> Int -> Int -> Integer
indexGrid grid x y = (grid !! y) !! x

iterF :: [Integer] -> Int -> [Integer]
iterF xs i = drop (i + 1) xs

iterB :: [Integer] -> Int -> [Integer]
iterB xs i = reverse (take i xs)

gridCol :: [[Integer]] -> Int -> [Integer]
gridCol grid x = [indexGrid grid x y | y <- [0..l]]
  where l = (length grid) - 1

check :: [Integer] -> Integer -> Bool
check (x:xs) v
  | v < x = False
  | otherwise = if xs == [] then True else check xs v

checkNorth :: [[Integer]] -> Int -> Int -> Bool
checkNorth grid x y = check (iterB (gridCol grid x) y) (indexGrid grid x y)
