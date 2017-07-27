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
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericDecodeJSON, genericEncode, genericEncodeJSON)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Data.String (take, toUpper)


newtype Clue = Clue {
  clue :: String,
  answer :: String
}

mkClue :: String -> String -> Clue
mkClue c a = Clue { clue: c, answer: a }

newtype Puzzle = Puzzle {
  quote :: String,
  numRows :: Int,
  clues :: Array Clue
}

mkPuzzle :: forall t. Foldable t => String -> Int -> t Clue -> Puzzle
mkPuzzle q r c = Puzzle { quote: q, numRows: r, clues: fromFoldable c }

answers :: Puzzle -> Array String
answers (Puzzle p) = (_.answer <<< unwrap) <$> p.clues

acronym :: forall t. Foldable t => t String -> String
acronym = foldMap (toUpper <<< take 1)

-- | Derived as acronym of the answers
source :: Puzzle -> String
source = answers >>> acronym


derive instance newtypeClue :: Newtype Clue _

derive instance genericClue :: Generic Clue _

instance showClue :: Show Clue where show = genericShow

instance encodeClue :: Encode Clue where encode = genericEncode opts

instance decodeClue :: Decode Clue where decode = genericDecode opts


derive instance newtypePuzzle :: Newtype Puzzle _

derive instance genericPuzzle :: Generic Puzzle _

instance showPuzzle :: Show Puzzle where show = genericShow

opts :: Options
opts = defaultOptions { unwrapSingleConstructors = true }

toJson :: Puzzle -> String
toJson = genericEncodeJSON opts

fromJson :: String -> Either MultipleErrors Puzzle
fromJson json = runExcept $ genericDecodeJSON opts json
