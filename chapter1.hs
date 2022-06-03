divides :: Integer -> Integer -> Bool
ld :: Integer -> Integer 
ldf :: Integer -> Integer -> Integer
prime0 :: Integer -> Bool

divides d n = rem n d == 0

-- least divisor of n that is >= k
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

-- least divisor of n is defined as the 
-- least divisor of n that is >= 2
ld n = ldf 2 n

-- primality test
-- a number greater than 1
-- is prime iff it is its own least
-- divisor
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

-- lists

min' :: Int -> Int -> Int
min' x y | x <= y    = x
         | otherwise = y

mmInt :: [Int] -> Int
mmInt [] = error "empty list"
mmInt [x] = x
mmInt (x:xs) = min x (mmInt xs)

-- max of list of integers
mxInt :: [Int] -> Int
mxInt [] = error "empty list"
mxInt [x] = x
mxInt (x:xs) = max x (mxInt xs)

-- remove an element from list
removeFst :: Int -> [Int] -> [Int]
removeFst n [] = []
removeFst n [x] | n == x    = []
                | otherwise = [x]
removeFst n (x:xs) | n == x    = xs
                   | otherwise = x : removeFst n xs

-- sort ints
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mmInt xs

-- sort ints with let
srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let
                 m = mmInt xs
                 in m : (srtInts' (removeFst m xs))

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- calculating some average number of the values of a list
average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum' xs) / fromIntegral (length' xs)
