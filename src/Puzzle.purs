module Puzzle (
  CharType(..),
  Clue(..),
  Puzzle(..),
  answers,
  cleanQuote,
  defaultPuzzle,
  fromJson,
  lettersRemaining,
  mkClue,
  mkPuzzle,
  source,
  toJson
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (fromFoldable, mapMaybe)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldMap)
import Data.Foreign (MultipleErrors)
import Data.Group (ginverse)
import Data.Maybe (Maybe(..))
import Data.Multiset as MS
import Data.String (singleton, take, toCharArray, toUpper)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
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



data CharType = Letter Char | Punct Char | Space


re :: String -> Regex
re s = unsafeRegex s global

letters :: Regex
letters = re $ "[A-Z]"

punctuation :: Regex
punctuation = re $ "[,!@#$%^&*().:;-]"

charType :: Char -> Maybe CharType
charType c | test letters (singleton c) = Just $ Letter c
charType c | test punctuation (singleton c) = Just $ Punct c
charType c | c == ' ' = Just Space
charType c | otherwise = Nothing


-- | Removes chars that shouldn't be displayed in the board.
cleanQuote :: String -> Array CharType
cleanQuote q = mapMaybe charType $ toCharArray $ toUpper q

countLetters :: String -> MS.Multiset Char
countLetters s = MS.fromFoldable $ mapMaybe lettersOnly $ cleanQuote s where
  lettersOnly (Letter c) = Just c
  lettersOnly _ = Nothing

-- | Counts letters in `p.quote` minus letters in `answers p`.
lettersRemaining :: Puzzle -> MS.Multiset Char
lettersRemaining p =
  countLetters p.quote <> ginverse (foldMap countLetters $ answers p)


toJson :: Puzzle -> String
toJson = writeJSON

fromJson :: String -> Either MultipleErrors Puzzle
fromJson json = runExcept $ readJSON json

defaultPuzzle :: Puzzle
defaultPuzzle = {
  quote: "The only thing we have to fear is fear itself.",
  numCols: 12,
  clues: [
    mkClue "Snitch" "rat",
    mkClue "\"Lay _ me, I'm starving!\"" "off",
    mkClue "Common shower gift" "onesie",
    mkClue "Reason to stay home" "shy",
    mkClue "Ambivalent reply" "either",
    mkClue "Bland dessert, perhaps" "vegan",
    mkClue "Will Ferrell role" "elf",
    mkClue "Some soccer fields" "lit",
    mkClue "A thing to do in spring" "thaw"
  ]
}
