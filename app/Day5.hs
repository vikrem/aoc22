{-# LANGUAGE RankNTypes #-}

module Day5 where

import Relude.Extra.Lens
import Relude.Unsafe as U
import Text.Parsec hiding ((<|>), State)
import Text.Parsec.Text
import Data.List (iterate')

day5 :: IO ()
day5 = do
  input <- decodeUtf8 <$> readFileBS "input/day5.txt"
  let parsed = runParser parser () "input" input
  case parsed of
    Left err -> print err
    Right (crates, cmds) -> do 
      print $ partOne crates cmds
      print $ partTwo crates cmds

crate :: Parser (Maybe Char)
crate =
  char '[' *> (Just <$> letter) <* char ']'
    <|> (replicateM_ 3 space $> Nothing)

parseLine :: Parser [Maybe Char]
parseLine = crate `sepBy1` char ' ' <* newline

skipLine :: Parser ()
skipLine = manyTill anyChar newline >> newline $> ()

parseCommand :: Parser (Int, (Int, Int))
parseCommand = do
  string "move "
  mul <- read <$> many1 digit
  string " from "
  a <- read <$> many1 digit
  string " to "
  b <- read <$> many1 digit
  pure (mul, (a, b))

parseCrates :: Parser [[Char]]
parseCrates = do
  lines <- replicateM 8 parseLine
  pure $ catMaybes <$> transpose lines

parser :: Parser ([[Char]], [(Int, (Int, Int))])
parser = do
  crates <- parseCrates
  skipLine
  commands <- parseCommand `sepBy1` newline
  pure (crates, commands)

partOne :: [[Char]] -> [(Int, (Int, Int))] -> String
partOne crates commands = U.head <$> execState (traverse_ stepOne commands) crates

partTwo :: [[Char]] -> [(Int, (Int, Int))] -> String
partTwo crates commands = U.head <$> execState (traverse_ stepTwo commands) crates

stepOne :: (Int, (Int, Int)) -> State [[Char]] ()
stepOne (mul, (a, b)) = do
  xs <- get
  let xs' = iterate' (move (a-1) (b-1)) xs !! mul
  put xs'

stepTwo :: (Int, (Int, Int)) -> State [[Char]] ()
stepTwo (mul, (a, b)) = do
  stepOne (mul, (a, b))
  xs <- get
  let xs' = over (ix (b-1)) (\ts -> reverse (take mul ts) <> drop mul ts) xs
  put xs'

ix :: Int -> Lens' [a] a
ix i = lens get set
  where
    get = (!! i)
    set xs x = take i xs <> [x] <> drop (i + 1) xs

move :: Int -> Int -> [[a]] -> [[a]]
move a b xs =
  let (h : r) = view (ix a) xs
   in xs
        & set (ix a) r
        & over (ix b) (h :)