toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0     = []
  | otherwise  = (mod n 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev (x1:(x2:xs)) = x1:((2 * x2):(doubleEveryOtherRev xs))
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev [] = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10     = x + sumDigits xs
  | otherwise  = (sumDigits (toDigits x)) + sumDigits xs

validate :: Integer -> Bool
validate n = (mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0

type Peg = String
type Move = (Peg, Peg)
-- hanoi num_disks peg1_name peg2_name peg3_name


-- return a list of moves to be performed to move the stack of
-- discs from the first peg to the second.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)
