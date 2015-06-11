module Golf where

-- include every nth element of the list
skipsN :: [a] -> Int -> [a]
skipsN [] _ = []

skipsN xs n =
  case drop (n - 1) xs of
   [] -> []
   (y:ys) -> y : skipsN ys n

skips :: [a] -> [[a]]
skips list = map (skipsN list) [1..length list]

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | x2 > x1 && x2 > x3 = x2 : localMaxima (x2:x3:xs)
    | otherwise = localMaxima (x2:x3:xs)
localMaxima _ = []

counts :: [Integer] -> [Int]
counts xs = map (\n -> length (filter (n ==) xs)) [0..9]

renderRow :: [Int] -> Int -> String
renderRow countList v = map (\a -> if a >= v then '*' else ' ') countList ++ "\n"

renderChart :: [Int] -> String
renderChart countList = foldl1 (++) (map (renderRow countList) [topValue,topValue-1..0])
  where topValue = maximum countList

histogram :: [Integer] -> String
histogram numbers = renderChart (counts numbers) ++ "==========\n0123456789\n"

