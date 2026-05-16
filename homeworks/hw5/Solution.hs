module Main where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

-- Exercise 1
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
    deriving (Show)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
    stack <- get
    case stack of
        (_:rest) -> put rest
        [] -> return ()
execInstr DUP = do
    stack <- get
    case stack of
        (x:_) -> modify (x :)
        [] -> return ()
execInstr SWAP = do
    stack <- get
    case stack of
        (x:y:rest) -> put (y : x : rest)
        _ -> return ()
execInstr ADD = do
    stack <- get
    case stack of
        (x:y:rest) -> put (x + y : rest)
        _ -> return ()
execInstr MUL = do
    stack <- get
    case stack of
        (x:y:rest) -> put (x * y : rest)
        _ -> return ()
execInstr NEG = do
    stack <- get
    case stack of
        (x:rest) -> put (negate x : rest)
        [] -> return ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []


-- Exercise 2
data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq Expr Expr
    deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n
eval (Var x) = do
    env <- get
    return (env Map.! x)
eval (Add l r) = do
    a <- eval l
    b <- eval r
    return (a + b)
eval (Mul l r) = do
    a <- eval l
    b <- eval r
    return (a * b)
eval (Neg e) = do
    v <- eval e
    return (negate v)
eval (Assign name e) = do
    v <- eval e
    modify (Map.insert name v)
    return v
eval (Seq l r) = do
    _ <- eval l
    eval r

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty


-- Exercise 3
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just v -> return v
        Nothing -> do
            result <-
                if i == 0 then
                    return j
                else if j == 0 then
                    return i
                else if xs !! (i - 1) == ys !! (j - 1) then
                    editDistM xs ys (i - 1) (j - 1)
                else do
                    d1 <- editDistM xs ys (i - 1) j
                    d2 <- editDistM xs ys i (j - 1)
                    d3 <- editDistM xs ys (i - 1) (j - 1)
                    return (1 + minimum [d1, d2, d3])

            modify (Map.insert (i, j) result)
            return result

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty


-- Exercises 4-6
data Location
    = StartLoc { nextLoc :: Int }
    | PathLoc { pathDesc :: String, nextLoc :: Int }
    | DecisionLoc { decDesc :: String, choices :: [(String, Int)] }
    | ObstacleLoc { obsDesc :: String, energyCost :: Int, nextLoc :: Int }
    | TreasureLoc { treDesc :: String, treValue :: Int, nextLoc :: Int }
    | TrapLoc { trapDesc :: String, trapPenalty :: Int, nextLoc :: Int }
    | GoalLoc
    deriving (Show)

data GameState = GameState
    { playerPos :: Int
    , energy :: Int
    , score :: Int
    , gameBoard :: Map Int Location
    } deriving (Show)

type AdventureGame a = StateT GameState IO a


buildBoard :: Map Int Location
buildBoard = Map.fromList
    [ (0,  StartLoc 1)
    , (1,  PathLoc "A dirt road stretches ahead." 2)
    , (2,  DecisionLoc "The path splits!"
            [("forest", 3), ("cave", 6)])
    , (3,  TreasureLoc "Golden coins" 15 4)
    , (4,  ObstacleLoc "A fallen tree blocks the way" 2 5)
    , (5,  PathLoc "The paths merge together." 8)
    , (6,  TrapLoc "You fell into a hidden pit!" 10 7)
    , (7,  TreasureLoc "Ancient amulet" 20 5)
    , (8,  DecisionLoc "A river blocks your way!"
            [("bridge", 9), ("swim", 11)])
    , (9,  ObstacleLoc "The bridge is old and rickety" 3 10)
    , (10, TreasureLoc "Emerald gem" 25 13)
    , (11, TrapLoc "The current pulls you under!" 15 12)
    , (12, PathLoc "You crawl to shore, exhausted." 13)
    , (13, TreasureLoc "Silver crown" 30 14)
    , (14, GoalLoc)
    ]

getDiceRoll :: IO Int
getDiceRoll = do
    putStr "  >> Roll the dice (1-6): "
    hFlush stdout
    line <- getLine
    case reads line of
        [(n, "")] | n >= 1 && n <= 6 -> return n
        _ -> do
            putStrLn "  !! Invalid input. Enter a number between 1 and 6."
            getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
    putStrLn ("  Position: " ++ show (playerPos gs)
            ++ " | Energy: " ++ show (energy gs)
            ++ " | Score: " ++ show (score gs))

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "  Choose your path:"
    mapM_ (\(i, opt) -> putStrLn ("    " ++ show i ++ ") " ++ opt)) (zip [1 :: Int ..] options)
    putStr "  >> Your choice: "
    hFlush stdout
    line <- getLine
    case reads line of
        [(n, "")] | n >= 1 && n <= length options ->
            return (options !! (n - 1))
        _ -> do
            putStrLn "  !! Invalid choice. Try again."
            getPlayerChoice options


movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
    moved <- go roll 0
    modify (\s -> s { energy = energy s - moved })
    pos <- gets playerPos
    lift $ putStrLn ("  You moved " ++ show moved ++ " steps to position " ++ show pos ++ ".")
    return moved
    where
        go 0 acc = return acc
        go n acc = do
            gs <- get
            case Map.lookup (playerPos gs) (gameBoard gs) of
                Just GoalLoc -> return acc
                Just (DecisionLoc _ _) -> return acc
                Just loc -> do
                    modify (\s -> s { playerPos = nextLoc loc })
                    go (n - 1) (acc + 1)
                Nothing -> return acc

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- lift (getPlayerChoice options)
    lift $ putStrLn ("  You chose: " ++ choice ++ "!")
    return choice


handleLocation :: AdventureGame Bool
handleLocation = do
    gs <- get
    case Map.lookup (playerPos gs) (gameBoard gs) of
        Nothing -> return False

        Just (StartLoc _) -> do
            lift $ putStrLn "  You stand at the entrance of the dungeon."
            return False

        Just (PathLoc desc _) -> do
            lift $ putStrLn ("  " ++ desc)
            return False

        Just (DecisionLoc desc opts) -> do
            lift $ putStrLn ("  " ++ desc)
            choice <- makeDecision (map fst opts)
            case lookup choice opts of
                Just dest -> modify (\s -> s { playerPos = dest })
                Nothing -> return ()
            handleLocation

        Just (ObstacleLoc desc cost _) -> do
            lift $ putStrLn ("  [!] OBSTACLE: " ++ desc)
            lift $ putStrLn ("      You lose " ++ show cost ++ " extra energy!")
            modify (\s -> s { energy = energy s - cost })
            return False

        Just (TreasureLoc desc value _) -> do
            lift $ putStrLn ("  [*] TREASURE: " ++ desc)
            lift $ putStrLn ("      You gain " ++ show value ++ " points!")
            modify (\s -> s { score = score s + value })
            return False

        Just (TrapLoc desc penalty _) -> do
            lift $ putStrLn ("  [X] TRAP: " ++ desc)
            lift $ putStrLn ("      You lose " ++ show penalty ++ " points!")
            modify (\s -> s { score = max 0 (score s - penalty) })
            return False

        Just GoalLoc -> do
            lift $ putStrLn "  [!!!] You found the MAIN TREASURE! Congratulations!"
            return True

playTurn :: AdventureGame Bool
playTurn = do
    gs <- get
    if energy gs <= 0
        then do
            lift $ putStrLn "\n  You ran out of energy... Game over!"
            return True
        else do
            roll <- lift getDiceRoll
            _ <- movePlayer roll
            finished <- handleLocation
            gs' <- get
            lift $ displayGameState gs'

            if finished
                then return True
                else if energy gs' <= 0
                    then do
                        lift $ putStrLn "\n  You ran out of energy... Game over!"
                        return True
                    else return False

playGame :: AdventureGame ()
playGame = do
    lift $ putStrLn ""
    lift $ putStrLn "       TREASURE HUNTERS"
    lift $ putStrLn ""
    gs <- get
    lift $ displayGameState gs
    loop
    where
        loop = do
            ended <- playTurn
            if ended
                then do
                    gs <- get
                    lift $ putStrLn ("   FINAL SCORE: " ++ show (score gs) ++ " ")
                else loop



main :: IO ()
main = do
  putStrLn "Exercise 1: Stack machine"
  print (runProg [PUSH 3, PUSH 4, ADD])
  print (runProg [PUSH 5, PUSH 3, MUL])
  print (runProg [PUSH 10, PUSH 3, PUSH 2, ADD, MUL])
  print (runProg [PUSH 5, DUP, ADD])
  print (runProg [PUSH 1, PUSH 2, SWAP])
  print (runProg [PUSH 7, NEG])
  print (runProg [PUSH 1, PUSH 2, PUSH 3, POP, ADD])
  print (runProg [POP, ADD])

  putStrLn "\nExercise 2: Expression evaluator"
  print (runEval (Num 42))
  print (runEval (Add (Num 2) (Num 3)))
  print (runEval (Mul (Num 4) (Num 5)))
  print (runEval (Neg (Num 7)))
  print (runEval (Seq (Assign "x" (Num 10)) (Var "x")))
  print (runEval (Seq (Assign "x" (Num 5)) (Add (Var "x") (Num 3))))
  print (runEval (Seq (Assign "x" (Num 2)) (Seq (Assign "y" (Num 3)) (Mul (Var "x") (Var "y")))))
  print (runEval (Seq (Assign "a" (Num 10)) (Seq (Assign "a" (Add (Var "a") (Num 5))) (Var "a"))))

  putStrLn "\nExercise 3: Edit distance"
  print (editDistance "kitten" "sitting")
  print (editDistance "hello" "hello")
  print (editDistance "" "abc")
  print (editDistance "abc" "")
  print (editDistance "saturday" "sunday")

  putStrLn "\nExercises 4-6: Treasure Hunters"
  putStr "Start the game? (y/n): "
  hFlush stdout
  answer <- getLine
  case answer of
    "y" -> do
      let initialState = GameState
            { playerPos = 0
            , energy    = 15
            , score     = 0
            , gameBoard = buildBoard
            }
      _ <- runStateT playGame initialState
      return ()
    _ -> putStrLn "Game skipped."
