module Puzzle (
  Clue(..),
  Puzzle(..),
  answers,
  fromJson,
  mkClue,
  mkPuzzle,
  source,
  toJson
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap)
import Data.Foreign (MultipleErrors)
import Data.String (take, toUpper)
import Simple.JSON (readJSON, writeJSON)


type Clue = {
  clue :: String,
  answer :: String
}

type Puzzle = {
  quote :: String,
  numCols :: Int,
  clues :: Array Clue
}

mkClue :: String -> String -> Clue
mkClue c a = { clue: c, answer: a }

mkPuzzle :: forall t. Foldable t => String -> Int -> t Clue -> Puzzle
mkPuzzle q cols c = { quote: q, numCols: cols, clues: fromFoldable c }

answers :: Puzzle -> Array String
answers p = _.answer <$> p.clues

acronym :: forall t. Foldable t => t String -> String
acronym = foldMap (toUpper <<< take 1)

-- | Derived as acronym of the answers
source :: Puzzle -> String
source p = acronym $ answers p

toJson :: Puzzle -> String
toJson = writeJSON

fromJson :: String -> Either MultipleErrors Puzzle
fromJson json = runExcept $ readJSON json
