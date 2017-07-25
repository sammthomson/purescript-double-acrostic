module Main where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array as Arr
import Data.Foldable (foldMap, foldr, traverse_)
import Data.Int (ceil, toNumber)
import Data.List as L
import Data.List.Lazy as LL
import Data.Multiset as MS
import Data.String (Pattern(..), singleton, split, take, toCharArray, toUpper)
import Data.String.Regex (Regex, replace, test)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Flare (UI, intSlider, resizableList, string, textarea)
import Flare.Smolder (runFlareHTML)
import Puzzle (Clue, Puzzle, answers, mkClue, mkPuzzle, source)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup as M


type Markup = M.Markup Unit

countChars :: String -> MS.Multiset Char
countChars s = MS.fromFoldable $ toCharArray $ replace notLetters "" $ toUpper s

punctuationStr :: String
punctuationStr = ",!@#$%^&*().:;-"

punctuation :: Regex
punctuation = unsafeRegex ("[" <> punctuationStr <> "]") global

lettersStr :: String
lettersStr = "A-Z"

notLetters :: Regex
notLetters = unsafeRegex ("[^" <> lettersStr <> "]") global

notDisplayableChar :: Regex
notDisplayableChar = unsafeRegex ("[^ " <> lettersStr <> punctuationStr <> "]") global

-- | Removes chars that shouldn't be displayed in the board.
cleanQuote :: String -> String
cleanQuote q = replace notDisplayableChar "" $ toUpper q

-- | Arranges `quote` into `numRows` roughly equal rows (the last row might
-- | be shorter).
formatQuote :: String -> Int -> Array (Array (Tuple Int Char))
formatQuote quote numRows =
  Arr.fromFoldable $ LL.take numRows $ rows (Arr.mapWithIndex Tuple chars)
  where
    chars = toCharArray $ cleanQuote quote
    numCols = ceil $ (toNumber $ Arr.length chars) / (toNumber numRows)
    rows cs = LL.cons (Arr.take numCols cs) (defer \_ ->
                                              rows $ Arr.drop numCols cs)

lettersRemaining :: String -> L.List String -> MS.Multiset Char
lettersRemaining quote clues =
  foldr MS.delete (countChars quote) (clues >>= (L.fromFoldable <<< toCharArray <<< toUpper))


renderCharCount :: MS.Multiset Char -> Markup
renderCharCount cs =
  H.table $ traverse_ renderChar $ MS.toUnfoldable cs :: LL.List _ where
    renderChar (Tuple c i) = H.tr $ do
            td $ M.text $ singleton c <> ":"
            td $ M.text $ show i where
              td = if i < 0 then H.td ! A.className "err" else H.td

-- | Renders a single char in the board.
cell :: Tuple Int Char -> Markup
cell (Tuple i c) =
  td $ do
    H.div ! A.className "idx" $ M.text $ show (i + 1)
    H.div ! A.className "letter" $ M.text $ singleton c
    where td =
            if c == ' ' then
               H.td ! A.className "blank"
            else if test punctuation (singleton c) then
               H.td ! A.className "punct"
            else
               H.td

-- | Renders the board.
renderBoard :: String -> Int -> Markup
renderBoard quote numRows =
  H.table $ traverse_ row $ formatQuote quote numRows
    where row chars = H.tr $ traverse_ cell chars


renderPuzzle :: Puzzle -> Markup
renderPuzzle puzzle =
  let
    boardId = "board"
    authorId = "author"
    charsId = "chars-left"
    charsLeft = lettersRemaining puzzle.quote (answers puzzle)
  in do
    H.div $ do
      -- H.label ! A.for boardId $ M.text "Board:"
      (renderBoard puzzle.quote puzzle.numRows) ! A.id boardId
    H.div $ do
      H.label ! A.for authorId $ M.text "Source:"
      H.span ! A.id authorId $ M.text $ source puzzle
    H.div $ do
      H.label ! A.for charsId $ M.text "Letters remaining:"
      (renderCharCount charsLeft) ! A.id charsId


clueUi :: forall e. Clue -> UI e Clue
clueUi clue = mkClue <$> string "Clue:" clue.clue
                     <*> string "Answer:" clue.answer

cluesUi :: forall e. L.List Clue -> UI e (L.List Clue)
cluesUi clues = resizableList "Clues:" clueUi emptyClue clues where
  emptyClue = { clue: "", answer: "" }


puzzleUi :: forall e. Puzzle -> UI e Puzzle
puzzleUi puzzle = mkPuzzle <$> textarea "Quote:" puzzle.quote
                           <*> intSlider "Rows:" 1 10 puzzle.numRows
                           <*> cluesUi puzzle.clues

acr :: forall e. UI e Markup
acr = renderPuzzle <$> puzzleUi defaultPuzzle where
    defaultPuzzle = {
      quote: "The only thing we have to fear is fear itself.",
      numRows: 4,
      clues: L.fromFoldable defaultClues
    }
    defaultClues = mkClue "" <$> split (Pattern "") "franklinroosevelt"


main ∷ forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = do
  runFlareHTML "controls" "board" acr
