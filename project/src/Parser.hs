module Parser
  ( parseScore
  , Parser
  , runParser
  , pitch
  , duration
  , noteItem
  , restItem
  , scoreItem
  , part
  , score
  ) where

import Types
import Control.Monad.State
import Data.Char (isDigit, isAlpha, isSpace, toUpper)

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

zero :: Parser a
zero = StateT (const [])

item :: Parser Char
item = do
  s <- get
  case s of
    c : cs -> put cs >> pure c
    [] -> zero

infixr 5 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = StateT $ \s ->
  case runStateT p1 s of
    [] -> runStateT p2 s
    parses -> parses

sat :: (Char -> Bool) -> Parser Char
sat predicate = do
  c <- item
  if predicate c then pure c else zero

char :: Char -> Parser Char
char c = sat (== c)

digit :: Parser Char
digit = sat isDigit

letterP :: Parser Char
letterP = sat isAlpha

spaceP :: Parser Char
spaceP = sat isSpace

string :: String -> Parser String
string [] = pure []
string (c : cs) = char c >> string cs >> pure (c : cs)

many :: Parser a -> Parser [a]
many p = many1 p <||> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many p
  pure (x : xs)

spaces :: Parser ()
spaces = many spaceP >> pure ()

skipWs :: Parser ()
skipWs = do
  spaces
  (do _ <- string "--"
      _ <- many (sat (/= '\n'))
      skipWs)
    <||> pure ()

token :: Parser a -> Parser a
token p = do { v <- p; skipWs; pure v }

symbol :: String -> Parser String
symbol cs = token (string cs)

nat :: Parser Int
nat = do
  ds <- many1 digit
  pure (read ds)

signedInt :: Parser Int
signedInt = token $ do
  sign <- (char '-' >> pure negate)
    <||>  (char '+' >> pure id)
    <||>  pure id
  n <- nat
  pure (sign n)

quotedString :: Parser String
quotedString = do
  _ <- char '"'
  s <- many (sat (/= '"'))
  _ <- char '"'
  pure s

identifier :: Parser String
identifier = many1 letterP

pitch :: Parser Pitch
pitch = do
  l <- sat (\c -> toUpper c `elem` "CDEFGAB")
  acc <- (char '#' >> pure 1)
    <||> (char 'b' >> pure (-1))
    <||> pure 0
  o <- nat
  pure (Pitch (toUpper l) acc o)

duration :: Parser Duration
duration =
      (char 'w' >> pure Whole)
  <||> (char 'h' >> pure Half)
  <||> (char 'q' >> pure Quarter)
  <||> (char 'e' >> pure Eighth)
  <||> (char 's' >> pure Sixteenth)

noteItem :: Parser Item
noteItem = do
  p <- token pitch
  d <- token duration
  pure (Note p d)

restItem :: Parser Item
restItem = do
  _ <- token (char 'r')
  d <- token duration
  pure (Rest d)

repeatItem :: Parser Item
repeatItem = do
  _ <- symbol "repeat"
  n <- token nat
  _ <- symbol "{"
  items <- many scoreItem
  _ <- symbol "}"
  pure (Repeat n items)

groupItem :: Parser Item
groupItem = do
  _ <- symbol "group"
  _ <- symbol "{"
  items <- many scoreItem
  _ <- symbol "}"
  pure (Group items)

transposeItem :: Parser Item
transposeItem = do
  _ <- symbol "transpose"
  k <- signedInt
  _ <- symbol "{"
  items <- many scoreItem
  _ <- symbol "}"
  pure (Transpose k items)

scoreItem :: Parser Item
scoreItem =
      repeatItem
  <||> groupItem
  <||> transposeItem
  <||> restItem
  <||> noteItem

part :: Parser Part
part = do
  _ <- symbol "instrument"
  name <- token identifier
  _ <- symbol "{"
  items <- many scoreItem
  _ <- symbol "}"
  pure (Part name items)

score :: Parser Score
score = do
  _ <- symbol "song"
  name <- token quotedString
  _ <- symbol "tempo"
  bpm <- token nat
  _ <- symbol "{"
  ps <- many1 part
  _ <- symbol "}"
  pure (Score name bpm ps)

parseScore :: String -> Either String Score
parseScore input =
  case runParser (skipWs >> score) input of
    ((s, "") : _) -> Right s
    ((_, rest) : _) ->
      Left ("Parse error: unexpected input near: " ++ take 30 rest)
    [] ->
      Left "Parse error: could not parse score"
