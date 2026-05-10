module Main where

-- Exercise 1

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
    pure x = Reader (\_ -> x)
    liftA2 f (Reader ra) (Reader rb) = Reader (\r -> f (ra r) (rb r))

instance Monad (Reader r) where
    (Reader ra) >>= f = Reader (\r -> runReader (f (ra r)) r)


-- Exercise 2

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (ra . f)


-- Exercise 3

data BankConfig = BankConfig
    { interestRate   :: Double
    , transactionFee :: Int
    , minimumBalance :: Int
    } deriving (Show)

data Account = Account
    { accountId :: String
    , balance   :: Int
    } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
    rate <- asks interestRate
    return (floor (fromIntegral (balance acc) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
    fee <- asks transactionFee
    return acc { balance = balance acc - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
    minBal <- asks minimumBalance
    return (balance acc >= minBal)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
    updatedAcc <- applyTransactionFee acc
    interest <- calculateInterest acc
    meetsMin <- checkMinimumBalance acc
    return (updatedAcc, interest, meetsMin)



main :: IO ()
main = do
  let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
  let acc = Account { accountId = "A-001", balance = 1000 }

  putStrLn "Exercise 1-2: Reader"
  print (runReader (pure 42 :: Reader String Int) "ignored")
  print (runReader (fmap (+1) (asks length)) "hello")
  print (runReader ask "environment")
  print (runReader (asks length) "hello")
  print (runReader (local (++"!") ask) "hello")
  print (runReader (liftA2 (+) (asks length) (pure 10)) "hello")

  putStrLn "Exercise 3: Banking"
  print (runReader (calculateInterest acc) cfg)
  print (runReader (applyTransactionFee acc) cfg)
  print (runReader (checkMinimumBalance acc) cfg)
  print (runReader (processAccount acc) cfg)

  let poorAcc = Account { accountId = "B-002", balance = 50 }
  print (runReader (processAccount poorAcc) cfg)

  let cfg2 = BankConfig { interestRate = 0.10, transactionFee = 5, minimumBalance = 200 }
  print (runReader (processAccount acc) cfg2)
