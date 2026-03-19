{-# LANGUAGE BangPatterns #-}
module Main where
    
-- Exercise 1
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | p <- primesTo n, let q = n - p, p <= q, isPrime q]

-- Exercise 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = go (removeDups xs)
  where
    go [] = []
    go (x:xs) = [(min x y, max x y) | y <- xs, gcd x y == 1] ++ go xs
    removeDups [] = []
    removeDups (x:xs) = x : removeDups (filter (/= x) xs)

-- Exercise 3
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2 .. n]

isPrime :: Int -> Bool
isPrime n = n > 1 && n `elem` primesTo n

-- Exercise 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
    let p = length (head a)
    in [[ sum [a !! i !! k * b !! k !! j | k <- [0..p-1]]
        | j <- [0..length (head b) - 1]]
        | i <- [0..length a - 1]]

-- Exercise 5
permutations :: Int -> [a] -> [[a]]
permutations k _ | k < 0 = []
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs = [x : rest | (x, remaining) <- select xs, rest <- permutations (k-1) remaining]
    where
        select [] = []
        select (y:ys) = (y, ys) : [(z, y:zs) | (z, zs) <- select ys]

-- Exercise 6
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x > y = y : merge (x:xs) ys
    | otherwise = x : merge xs ys
 
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))

-- Exercise 7
power :: Int -> Int -> Int
power _ e | e < 0 = error "negative exponent"
power b e = go e 1
    where
        go 0 !acc = acc
        go n !acc = go (n-1) (acc * b)

-- Exercise 8
listMaxSeq :: [Int] -> Int
listMaxSeq (x:xs) = go xs x
    where
        go [] acc = acc
        go (y:ys) acc = let m = max acc y in seq m (go ys m)
 
listMaxBang :: [Int] -> Int
listMaxBang (x:xs) = go xs x
    where
        go [] !acc = acc
        go (y:ys) !acc = go ys (max acc y)

-- Exercise 9
primes :: [Int]
primes = sieve [2..]
 
isPrime' :: Int -> Bool
isPrime' n = n > 1 && n == head (dropWhile (< n) primes)

-- Exercise 10

-- (a) 
meanLazy :: [Double] -> Double
meanLazy [] = error "empty list"
meanLazy xs = go xs 0 0
    where
        go [] s n = s / n
        go (x:xs) s n = go xs (s + x) (n + 1)
 
-- (b) 
meanStrict :: [Double] -> Double
meanStrict [] = error "empty list"
meanStrict xs = go xs 0 0
    where
        go [] !s !n = s / n
        go (x:xs) !s !n = go xs (s + x) (n + 1)

mean :: [Double] -> Double
mean = meanStrict
 
-- (c) 
meanVariance :: [Double] -> (Double, Double)
meanVariance [] = error "empty list" 
meanVariance xs = let (s, sq, n) = go xs 0 0 0 
                      mu = s / n
                in (mu, sq / n - mu * mu)
    where
        go [] !s !sq !n = (s, sq, n)
        go (x:xs) !s !sq !n = go xs (s + x) (sq + x*x) (n + 1)


main :: IO ()
main = do
    putStrLn "Exercise 1"
    print (goldbachPairs 10)
    print (goldbachPairs 28)
 
    putStrLn "Exercise 2"
    print (coprimePairs [3, 5, 8])
    print (coprimePairs [4, 6, 9])
    print (coprimePairs [5, 2, 3, 3])
 
    putStrLn "Exercise 3"
    print (primesTo 30)
    print (isPrime 17)
 
    putStrLn "Exercise 4"
    print (matMul [[1,2],[3,4]] [[5,6],[7,8]])
 
    putStrLn "Exercise 5"
    print (permutations 2 [1,2,3 :: Int])
 
    putStrLn "Exercise 6"
    print (take 20 hamming)
 
    putStrLn "Exercise 7"
    print (power 2 10)
 
    putStrLn "Exercise 8"
    print (listMaxSeq [3, 1, 7, 2, 9, 4])
    print (listMaxBang [3, 1, 7, 2, 9, 4])
 
    putStrLn "Exercise 9"
    print (take 15 primes)
    print (isPrime' 97)
 
    putStrLn "Exercise 10"
    print (meanLazy [1, 2, 3, 4, 5])
    print (mean [1, 2, 3, 4, 5])
    print (meanVariance [1, 2, 3, 4, 5])
