module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (permutations)
import Control.Monad (guard)

-- Exercise 1
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = Map.lookup pos maze >>= Map.lookup dir

-- (b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = Just pos
followPath maze pos (d:ds) = move maze pos d >>= \next -> followPath maze next ds

-- (c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = Just [pos]
safePath maze pos (d:ds) = do
    next <- move maze pos d
    rest <- safePath maze next ds
    return (pos : rest)


-- Exercise 2
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (\c -> Map.lookup c key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)


-- Exercise 3
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    perm <- permutations guests
    guard (valid perm)
    return perm
    where
        valid perm = all (not . adjacent perm) conflicts
        adjacent perm (a, b) = any (\(x, y) -> (x == a && y == b) || (x == b && y == a)) (pairs perm)
        pairs [] = []
        pairs perm = zip perm (tail perm ++ [head perm])


-- Exercise 4
data Result a = Failure String | Success a [String]
    deriving (Show)

-- (a)
instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success x ws) = Success (f x) ws

instance Applicative Result where
    pure x = Success x []
    Failure msg <*> _ = Failure msg
    _ <*> Failure msg = Failure msg
    Success f ws1 <*> Success x ws2 = Success (f x) (ws1 ++ ws2)

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success x ws >>= f = case f x of
        Failure msg -> Failure msg
        Success y ws' -> Success y (ws ++ ws')

-- (b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- (c)
validateAge :: Int -> Result Int
validateAge age
    | age < 0   = failure "Negative age"
    | age > 150 = do warn "Age is above 150"; return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge


-- Exercise 5
newtype Writer m a = Writer { runWriter :: (a, m) }
    deriving (Show)

instance Functor (Writer m) where
    fmap f (Writer (x, m)) = Writer (f x, m)

instance Monoid m => Applicative (Writer m) where
    pure x = Writer (x, mempty)
    Writer (f, m1) <*> Writer (x, m2) = Writer (f x, m1 <> m2)

instance Monoid m => Monad (Writer m) where
    Writer (x, m1) >>= f = let Writer (y, m2) = f x in Writer (y, m1 <> m2)

tell :: m -> Writer m ()
tell m = Writer ((), m)

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
    deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Neg e) = do
    e' <- simplify e
    case e' of
        Neg inner -> do tell ["Double negation: Neg (Neg e) -> e"]; return inner
        _ -> return (Neg e')

simplify (Add l r) = do
    l' <- simplify l
    r' <- simplify r
    case (l', r') of
        (Lit 0, _) -> do tell ["Add identity: 0 + e -> e"]; return r'
        (_, Lit 0) -> do tell ["Add identity: e + 0 -> e"]; return l'
        (Lit a, Lit b) -> do tell ["Constant fold: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a+b)]; return (Lit (a+b))
        _  -> return (Add l' r')

simplify (Mul l r) = do
    l' <- simplify l
    r' <- simplify r
    case (l', r') of
        (Lit 0, _) -> do tell ["Zero absorption: 0 * e -> 0"]; return (Lit 0)
        (_, Lit 0) -> do tell ["Zero absorption: e * 0 -> 0"]; return (Lit 0)
        (Lit 1, _) -> do tell ["Mul identity: 1 * e -> e"]; return r'
        (_, Lit 1) -> do tell ["Mul identity: e * 1 -> e"]; return l'
        (Lit a, Lit b) -> do tell ["Constant fold: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a*b)]; return (Lit (a*b))
        _ -> return (Mul l' r')


-- Exercise 6
newtype ZipList a = ZipList { getZipList :: [a] }
    deriving (Show)

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- ZipList cannot have a lawful Monad instance because >>= requires the function (a -> ZipList b) to produce a ZipList for each element.
-- If those ZipLists have different lengths, there is no consistent way to combine them positionally. 
-- The monad laws break because the resulting length depends on the function's output, not just the input structure



main :: IO ()
main = do
  let maze = Map.fromList
        [ ((0,0), Map.fromList [(E, (1,0)), (S, (0,1))])
        , ((1,0), Map.fromList [(W, (0,0)), (S, (1,1))])
        , ((0,1), Map.fromList [(N, (0,0)), (E, (1,1))])
        , ((1,1), Map.fromList [(N, (1,0)), (W, (0,1))])
        ]
  putStrLn "Exercise 1: Maze"
  print (move maze (0,0) E)
  print (move maze (0,0) N)
  print (followPath maze (0,0) [E, S])
  print (followPath maze (0,0) [N])
  print (safePath maze (0,0) [E, S])
  print (safePath maze (0,0) [E, N])
  print (followPath maze (0,0) [])
  print (safePath maze (0,0) [])
  print (move maze (9,9) E)

  let key = Map.fromList [('a','h'), ('b','e'), ('c','l'), ('d','o')]
  putStrLn "Exercise 2: Decrypt"
  print (decrypt key "abccd")
  print (decrypt key "abcx")
  print (decryptWords key ["abc", "ab"])
  print (decryptWords key ["abc", "abx"])
  print (decrypt key "")
  print (decryptWords key [])

  putStrLn "Exercise 3: Seatings"
  print (seatings ["A","B","C"] [("A","B")])
  print (seatings ["A","B","C","D"] [("A","B"),("C","D")])
  print (seatings ["A","B","C"] [])
  print (seatings [] [])
  print (seatings [] [("A","B")])

  putStrLn "Exercise 4: Result"
  print (validateAge 25)
  print (validateAge (-5))
  print (validateAge 200)
  print (validateAges [25, 30, 200])
  print (validateAges [25, -1, 30])
  print (validateAges [160, 170, 25])

  putStrLn "Exercise 5: Simplify"
  print (runWriter (simplify (Add (Lit 0) (Lit 5))))
  print (runWriter (simplify (Mul (Lit 1) (Add (Lit 2) (Lit 3)))))
  print (runWriter (simplify (Neg (Neg (Lit 7)))))
  print (runWriter (simplify (Mul (Lit 0) (Add (Lit 1) (Lit 2)))))
  print (runWriter (simplify (Add (Lit 5) (Lit 0))))
  print (runWriter (simplify (Mul (Lit 5) (Lit 1))))
  print (runWriter (simplify (Mul (Lit 5) (Lit 0))))
  print (runWriter (simplify (Mul (Lit 2) (Lit 3))))
  print (runWriter (simplify (Add (Lit 2) (Lit 3))))
  print (runWriter (simplify (Add (Lit 2) (Mul (Lit 3) (Lit 4)))))

  putStrLn "Exercise 6: ZipList"
  print (pure id <*> ZipList [1,2,3 :: Int])
  print (pure (+) <*> ZipList [1,2,3 :: Int] <*> ZipList [10,20,30])
