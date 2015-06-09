module Golf where


-- include every nth element of the list
skipsN :: [a] -> Int -> [a]
skipsN [] n = []
skipsN xs n = case drop (n - 1) xs of
              [] -> []
              (y:ys) -> y : (skipsN ys n)


skips :: [a] -> [[a]]
skips list = map (skipsN list) [1..length list]

-- XXX simplify

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
  | x2 > x1 && x2 > x3 = x2 : (localMaxima (x2:x3:xs))
  | otherwise = localMaxima (x2:x3:xs)
localMaxima _ = []
-- XXX filter with >

countOf :: [Integer] -> Integer -> Int
countOf xs n = length (filter ((==) n) xs)

counts :: [Integer] -> [Int]
counts xs = map (countOf xs) [0..9]

renderStarIfGTE :: Int -> Int -> Char
renderStarIfGTE value a = if a >= value then '*' else ' '

renderRow :: [Int] -> Int -> String
renderRow counts v = (map (renderStarIfGTE v) counts) ++ "\n"

renderChart :: [Int] -> String
renderChart counts = foldl1 (++) (map (renderRow counts) [topValue,topValue-1..0])
   where topValue = maximum counts

histogram :: [Integer] -> String
histogram numbers = (renderChart (counts numbers)) ++ "==========\n0123456789\n"
