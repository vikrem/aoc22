module Day1 where

import Data.List (maximum)
import Text.Parsec
import Text.Parsec.Text
import Text.Read

day1 :: IO ()
day1 = do
  input <- decodeUtf8 <$> readFileBS "input/day1.txt"
  let parsed = runParser elfParser () "input" input
  case parsed of
    Left err -> print err
    Right xs -> do
      print $ partOne xs
      print $ partTwo xs

partOne :: [[Int]] -> Int
partOne = maximum . fmap sum

partTwo :: [[Int]] -> Int
partTwo = sum . take 3 . reverse . sort . fmap sum

elfParser :: Parser [[Int]]
elfParser = do
  let elf = many1 digit `endBy` newline
  let elves = elf `sepBy` newline
  -- Parse characters to integers, throw out bad parses
  (fmap . fmap) (mapMaybe readMaybe) elves