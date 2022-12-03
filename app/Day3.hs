{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Day3 where

import Data.HashSet (intersection)
import qualified Data.Text as T
import Relude.Extra.Map
import qualified Relude.Unsafe as U
import Text.Parsec
import Text.Parsec.Text

day3 :: IO ()
day3 = do
  input <- decodeUtf8 <$> readFileBS "input/day3.txt"
  let parsed = runParser parser () "input" input
  case parsed of
    Left err -> print err
    Right xs -> do
      print $ partOne xs
      print $ partTwo xs

parser :: Parser [String]
parser = many1 letter `sepBy` newline

priority :: Char -> Int
priority c = lookupDefault 0 c d
  where
    d :: HashMap Char Int =
      fromList $
        zip ['a' .. 'z'] [1 ..] <> zip ['A' .. 'Z'] [27 ..]

partOne :: [String] -> Int
partOne xs = sum $ priority . overlap <$> xs

partTwo :: [String] -> Int
partTwo xs = sum $ priority . extraPart <$> chunksOf 3 xs

overlap :: String -> Char
overlap t =
  let len = length t
      (lhs, rhs) = splitAt (len `div` 2) t
      lhsMap :: HashSet Char = fromList lhs
      rhsMap :: HashSet Char = fromList rhs
      (Just u) = viaNonEmpty head $ toList $ intersection lhsMap rhsMap
   in u

extraPart :: [String] -> Char
extraPart xs =
  let maps = fmap (fromList @(HashSet Char)) xs
      fold = U.head . toList . foldr intersection (U.head maps)
   in fold maps

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)