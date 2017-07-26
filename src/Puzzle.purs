module Puzzle (
  Clue,
  Puzzle,
  answers,
  mkClue,
  mkPuzzle,
  source
  ) where

import Prelude
import Data.Foldable (foldMap)
import Data.List (List)
import Data.String (take, toUpper)


type Clue = {
  clue :: String,
  answer :: String
}

mkClue :: String -> String -> Clue
mkClue = { clue: _, answer: _ }

type Puzzle = {
  quote :: String,
  numRows :: Int,
  clues :: List Clue
}

mkPuzzle :: String -> Int -> List Clue -> Puzzle
mkPuzzle = { quote: _, numRows: _, clues: _ }

answers :: Puzzle -> List String
answers puzzle = _.answer <$> puzzle.clues

-- | Acronym of the answers
source :: Puzzle -> String
source puzzle = foldMap (toUpper <<< take 1) $ answers puzzle
