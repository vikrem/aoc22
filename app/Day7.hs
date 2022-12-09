{-# LANGUAGE RankNTypes #-}

module Day7 where

import Data.List (iterate', minimum, nub)
import Data.Map hiding (filter)
import Relude.Unsafe as U
import Text.Parsec hiding (Line, State, (<|>))
import Text.Parsec.Text

data Line = Command Command | Datum Datum
  deriving (Eq, Ord, Show)

data Command = Cd String | Ls
  deriving (Eq, Ord, Show)

data Datum = Directory String | File String Int
  deriving (Eq, Ord, Show)

data Tree = DirectoryT String (Map String Tree) | FileT String Int
  deriving (Eq, Ord, Show, Read)

day7 :: IO ()
day7 = do
  input <- decodeUtf8 <$> readFileBS "input/day7.txt"
  let (Right parsed) = runParser parser () "input" input
  partOne parsed
  partTwo parsed

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

mergeTrees :: Tree -> Tree -> Tree
mergeTrees (DirectoryT n a) (FileT s b) = DirectoryT n $ insert s (FileT s b) a
mergeTrees (DirectoryT n a) (DirectoryT s b) = DirectoryT n $ unionWith mergeTrees a b
mergeTrees _ _ = error $ toText "mergeTrees: invalid arguments"

mkTree :: [Datum] -> Tree
mkTree [] = DirectoryT "/" mempty
mkTree (Directory n : xs) = case mkTree xs of
  d@(DirectoryT n' _) -> DirectoryT n $ singleton n' d
  f@(FileT s _) -> DirectoryT n $ singleton s f
mkTree (File s i : _) = FileT s i

processCmd :: Command -> State ([Datum], Tree) ()
processCmd Ls = pure ()
processCmd (Cd s) = do
  (path, tree) <- get
  let path' = case s of
        "/" -> [Directory "/"]
        ".." -> case U.init path of
          [] -> [Directory "/"]
          x -> x
        _ -> path ++ [Directory s]
  put (path', tree)

processDatum :: Datum -> State ([Datum], Tree) ()
processDatum d@(Directory _) = pure ()
processDatum d = do
  (path, tree) <- get
  let tree' = mergeTrees tree $ mkTree $ path ++ [d]
  put (path, tree')

processLine :: Line -> State ([Datum], Tree) ()
processLine (Command c) = processCmd c
processLine (Datum d) = processDatum d

partOne :: [Line] -> IO ()
partOne lines = do
  let x@(pwd, tree) = execState (traverse_ processLine lines) ([Directory "/"], DirectoryT "/" mempty)
  print $ findSumsUnder100k tree

sumTree :: Tree -> Int
sumTree (DirectoryT _ m) = sum $ sumTree <$> elems m
sumTree (FileT _ i) = i

findSumsUnder100k :: Tree -> Int
findSumsUnder100k d@(DirectoryT _ m) =
  let s = sumTree d
      a = if s <= 100000 then s else 0
   in a + sum (findSumsUnder100k <$> elems m)
findSumsUnder100k (FileT _ _) = 0

findDeletionTarget :: Tree -> Int -> Int
findDeletionTarget (FileT _ i) x = 70000000
findDeletionTarget d@(DirectoryT _ m) t = minimum $ filter (>= t) $ sumTree d : (flip findDeletionTarget t <$> elems m)

partTwo :: [Line] -> IO ()
partTwo lines = do
  let x@(pwd, tree) = execState (traverse_ processLine lines) ([Directory "/"], DirectoryT "/" mempty)
  let used = sumTree tree
  let free = 70000000 - used
  let target = 30000000 - free
  print $ findDeletionTarget tree target