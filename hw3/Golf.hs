module Golf where


-- include every nth element of the list
skipsN :: [a] -> Int -> [a]
skipsN [] n = []
skipsN xs n = case drop (n - 1) xs of
              [] -> []
              (y:ys) -> y : (skipsN ys n)


skips :: [a] -> [[a]]
skips list = map (skipsN list) [1..length list]