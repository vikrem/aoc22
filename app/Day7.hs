{-# LANGUAGE RankNTypes #-}

module Day7 where

import Data.List (iterate', nub)
import Relude.Unsafe as U
import Text.Parsec hiding (Line, (<|>))
import Text.Parsec.Text

data Line = Command Command | Datum Datum
  deriving (Eq, Ord, Show)

data Command = Cd String | Ls
  deriving (Eq, Ord, Show)

data Datum = Directory String | File String Int
  deriving (Eq, Ord, Show)

day6 :: IO ()
day6 = do
  input <- decodeUtf8 <$> readFileBS "input/day7.txt"
  print $ runParser parser () "input" input
  print $ runParser parser () "input" input

parser :: Parser [Line]
parser = parseLine `sepBy1` newline

parseName :: Parser String
parseName = many1 (letter <|> digit <|> oneOf "-_/.")

parseDatum :: Parser Datum
parseDatum = do
  let dirParser = do
        string "dir "
        Directory <$> parseName
  let fileParser = do
        size <- U.read <$> many1 digit
        space
        name <- parseName
        pure $ File name size
  dirParser <|> fileParser

parseCommand :: Parser Command
parseCommand = do
  let cdParser = do
        string "$ cd "
        Cd <$> parseName
  let lsParser = do
        string "$ ls"
        pure Ls
  try cdParser <|> try lsParser

parseLine :: Parser Line
parseLine = do
  let commandParser = Command <$> parseCommand
  let datumParser = Datum <$> parseDatum
  commandParser <|> datumParser
