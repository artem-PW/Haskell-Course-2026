module Main where

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show, Eq)

-- Exercise 1
instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)


-- Exercise 2
instance Foldable Sequence where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldMap (:[])

seqLength :: Sequence a -> Int
seqLength = length


-- Exercise 3
instance Semigroup (Sequence a) where
    Empty <> x = x
    x <> Empty = x
    l <> r = Append l r

instance Monoid (Sequence a) where
    mempty = Empty


-- Exercise 4
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x s = go [s]
    where
        go [] = False
        go (Empty : stack) = go stack
        go (Single y : stack)
            | x == y = True
            | otherwise = go stack
        go (Append l r : stack) = go (l : r : stack)


-- Exercise 5
tailToList :: Sequence a -> [a]
tailToList s = go [s] []
    where
        go [] acc = reverse acc
        go (Empty : stack) acc = go stack acc
        go (Single x : stack) acc = go stack (x : acc)
        go (Append l r : stack) acc = go (l : r : stack) acc


-- Exercise 6
data Token = TNum Int | TAdd | TSub | TMul | TDiv
    deriving (Show)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
    where
        go [] [result] = Just result
        go [] _ = Nothing
        go (TNum n : rest) stack = go rest (n : stack)
        go (TAdd : rest) (b:a:stack) = go rest (a + b : stack)
        go (TSub : rest) (b:a:stack) = go rest (a - b : stack)
        go (TMul : rest) (b:a:stack) = go rest (a * b : stack)
        go (TDiv : rest) (b:a:stack)
            | b == 0 = Nothing
            | otherwise = go rest (a `div` b : stack)
        go _ _ = Nothing


-- Exercise 7

-- (a)
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

-- (b)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- (c)
decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0


-- Exercise 8

-- (a)
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
    where
        step x [] = [(x, 1)]
        step x ((y, n) : rest)
            | x == y = (y, n + 1) : rest
            | otherwise = (x, 1) : (y, n) : rest

-- (b)
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []



main :: IO ()
main = do
  let s1 = Append (Append (Single 1) (Single 2)) (Single 3)
  let s2 = Append (Single 4) (Single 5)
  let emptySeq = (Empty :: Sequence Int)
  let singleSeq = Single 42

  putStrLn "Exercise 1: Functor"
  print (fmap (*2) s1)
  print (fmap (+10) s2)
  print (fmap (+1) emptySeq)
  print (fmap (*3) singleSeq)

  putStrLn "Exercise 2: Foldable"
  print (seqToList s1)
  print (seqLength s1)
  print (seqToList emptySeq)
  print (seqLength emptySeq)
  print (seqToList singleSeq)
  print (seqLength singleSeq)

  putStrLn "Exercise 3: Semigroup / Monoid"
  print (seqToList (s1 <> s2))
  print (seqToList (mempty <> s1))
  print (seqToList (s1 <> mempty))
  print (seqToList (emptySeq <> s2))

  putStrLn "Exercise 4: tailElem"
  print (tailElem 2 s1)
  print (tailElem 5 s1)
  print (tailElem 42 singleSeq)
  print (tailElem 1 emptySeq)

  putStrLn "Exercise 5: tailToList"
  print (tailToList s1)
  print (tailToList s2)
  print (tailToList emptySeq)
  print (tailToList singleSeq)

  putStrLn "Exercise 6: tailRPN"
  print (tailRPN [TNum 3, TNum 4, TAdd])
  print (tailRPN [TNum 5, TNum 3, TSub])
  print (tailRPN [TNum 2, TNum 3, TMul, TNum 4, TAdd])
  print (tailRPN [TNum 10, TNum 0, TDiv])
  print (tailRPN [TNum 1, TNum 2])
  print (tailRPN [TAdd])

  putStrLn "Exercise 7: foldr / foldl"
  print (myReverse [1, 2, 3, 4, 5 :: Int])
  print (myReverse ([] :: [Int]))
  print (myTakeWhile even [2, 4, 3, 6 :: Int])
  print (myTakeWhile (< 10) [1, 2, 3, 4 :: Int])
  print (myTakeWhile even [1, 2, 4 :: Int])
  print (decimal [1, 2, 3])
  print (decimal [])

  putStrLn "Exercise 8: Run-length encoding"
  print (encode "aaabccca")
  print (encode "")
  print (decode [('a',3),('b',1),('c',3),('a',1)])
  print (decode ([] :: [(Char, Int)]))