module Day2 where

import Text.Parsec
import Text.Parsec.Text

day2 :: IO ()
day2 = do
  input <- decodeUtf8 <$> readFileBS "input/day2.txt"
  let parsed = runParser stratParser () "input" input
  case parsed of
    Left err -> print err
    Right xs -> do
      print $ partOne xs
      print $ partTwo xs

partOne :: [(Char, Char)] -> Int
partOne = sum . fmap (\(x, y) -> choicePts y + roundPts x y)

partTwo :: [(Char, Char)] -> Int
partTwo = sum . fmap
      ( \(x, y) ->
          let (Just r) = find' x y
           in choicePts r + roundPts x r
      )

stratParser :: Parser [(Char, Char)]
stratParser = flip sepBy1 newline $ do
  l <- letter
  space
  r <- letter
  return (l, r)

choicePts :: Char -> Int
choicePts 'X' = 1
choicePts 'Y' = 2
choicePts _ = 3

roundPts :: Char -> Char -> Int
roundPts 'A' 'X' = 3
roundPts 'A' 'Y' = 6
roundPts 'B' 'Y' = 3
roundPts 'B' 'Z' = 6
roundPts 'C' 'X' = 6
roundPts 'C' 'Z' = 3
roundPts _ _ = 0

desired :: Char -> Int
desired 'X' = 0
desired 'Y' = 3
desired _ = 6

find' :: Char -> Char -> Maybe Char
find' a b = viaNonEmpty head $ do
  r <- ['X' .. 'Z']
  guard $ roundPts a r == desired b
  pure r