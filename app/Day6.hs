{-# LANGUAGE RankNTypes #-}

module Day6 where

import Relude.Unsafe as U
import Text.Parsec
import Text.Parsec.Text
import Data.List (iterate', nub)

day6 :: IO ()
day6 = do
  input <- decodeUtf8 <$> readFileBS "input/day6.txt"
  print $ runParser (parser 4) () "input" input
  print $ runParser (parser 14) () "input" input

parser :: Int -> Parser Int
parser k = do
  x <- manyTill anyChar (try $ sop k)
  pure $ length x + k

sop :: Int -> Parser String
sop k = do
  s <- replicateM k letter
  guard $ length (nub s) == k
  pure s