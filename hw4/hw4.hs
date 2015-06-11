
import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . (map (subtract 2)) . (filter even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' = sum . filter even . takeWhile (>1) . iterate f
  where f n = if even n then (n `div` 2) else 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

treeHeight :: (Tree a) -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

addNode :: a -> (Tree a) -> (Tree a)
addNode newval Leaf = Node 0 Leaf newval Leaf
addNode newval (Node h left old right)
  | (treeHeight left) < (treeHeight right) =
     let newleft = (addNode newval left)
     in Node (succ (treeHeight newleft)) newleft old right
  | otherwise                              =
     let newright = (addNode newval right)
     in Node (succ (treeHeight newright)) left old newright

foldTree :: [a] -> Tree a
foldTree x = foldr addNode Leaf x 

xor :: [Bool] -> Bool
xor = foldl (\odd_num_trues v -> if v then not odd_num_trues else odd_num_trues) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\val acc -> (f val:acc)) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\n -> 2*n + 1) $ filter (\e -> notElem e toRemove) [1..n]
   where toRemove = filter (<=n) $ [i+j+2*i*j | i <- [1..n], j <- [1..n], i <= j]
