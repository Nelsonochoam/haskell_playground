module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- Dictionary

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/ dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do 
  aw <- allWords
  return (filter gameLength aw)
    where gameLength w = let l = length (w :: String) in l >= minWordLength && l < maxWordLength


randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO(0, length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

-- Puzzle

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
      fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar c =
  case c of
    Just char -> char
    Nothing -> '_'

freshPuzzle :: String ->  Puzzle
freshPuzzle w = Puzzle w [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) guess =
  elem guess word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ discovered guessed) guess =
  elem guess guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
    where zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
      newFilledSoFar = zipWith (zipper c) word filledInSoFar


main :: IO ()
main = do
  putStrLn "hello world"
