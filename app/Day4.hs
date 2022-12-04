module Day4 where

import Relude.Unsafe as U
import Text.Parsec
import Text.Parsec.Text

day4 :: IO ()
day4 = do
  input <- decodeUtf8 <$> readFileBS "input/day4.txt"
  let parsed = runParser parser () "input" input
  case parsed of
    Left err -> print err
    Right xs -> do
      print $ partOne xs
      print $ partTwo xs

partOne :: [((Int, Int), (Int, Int))] -> Int
partOne = countP isNested

partTwo :: [((Int, Int), (Int, Int))] -> Int
partTwo = countP isOverlapping

countP :: (a -> Bool) -> [a] -> Int
countP f xs = length . filter (== True) $ f <$> xs

pair :: Parser (Int, Int)
pair = do
  x <- read <$> many1 digit
  char '-'
  y <- read <$> many1 digit
  pure (x, y)

line :: Parser ((Int, Int), (Int, Int))
line = do
  p1 <- pair
  char ','
  p2 <- pair
  pure (p1, p2)

parser :: Parser [((Int, Int), (Int, Int))]
parser = line `sepBy` newline

isNested :: (Ord a1, Ord a2) => ((a1, a2), (a1, a2)) -> Bool
isNested ((a, b), (c, d)) = a <= c && d <= b || c <= a && b <= d

isOverlapping :: (Ord a1, Ord a2) => ((a1, a2), (a2, a1)) -> Bool
isOverlapping ((a, b), (c, d)) = not $ a > d || c > b
