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