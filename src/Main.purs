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
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup as M


type Markup = M.Markup Unit

type Clue = { clue :: String, answer :: String }

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


notLetterOrSpace :: Regex
-- notLetterOrSpace = unsafeRegex "[^ A-Z]" global
notLetterOrSpace = unsafeRegex ("[^ " <> lettersStr <> punctuationStr <> "]") global

cleanQuote :: String -> String
cleanQuote q = replace notLetterOrSpace "" $ toUpper q


formatQuote :: String -> Int -> Array (Array (Tuple Int Char))
formatQuote quote numRows =
  Arr.fromFoldable $ LL.take numRows $ rows (Arr.mapWithIndex Tuple chars)
  where
    chars = toCharArray $ cleanQuote quote
    numCols = ceil $ (toNumber $ Arr.length chars) / (toNumber numRows)
    rows cs = LL.cons (Arr.take numCols cs) (defer \_ ->
                                              rows $ Arr.drop numCols cs)

lettersRemaining :: MS.Multiset Char -> L.List String -> MS.Multiset Char
lettersRemaining quoteChars clues =
  foldr MS.delete quoteChars (clues >>= (L.fromFoldable <<< toCharArray <<< toUpper))


renderCharCount :: MS.Multiset Char -> Markup
renderCharCount cs =
  H.table $ traverse_ renderChar $ MS.toUnfoldable cs :: LL.List _ where
    renderChar (Tuple c i) = H.tr $ do
            td $ M.text $ singleton c <> ":"
            td $ M.text $ show i where
              td = if i < 0 then H.td ! A.className "err" else H.td


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


renderBoard :: String -> Int -> Markup
renderBoard quote numRows =
  H.table $ traverse_ row $ formatQuote quote numRows
    where row chars = H.tr $ traverse_ cell chars


renderAll :: String -> Int -> L.List Clue -> Markup
renderAll quote numRows clues =
  let
    boardId = "board"
    authorId = "author"
    charsId = "chars-left"
    clueAnswers = map _.answer clues
  in do
    H.div $ do
      H.label ! A.for boardId $ M.text "Board:"
      (renderBoard quote numRows) ! A.id boardId
    H.div $ do
      H.label ! A.for authorId $ M.text "Author:"
      H.span ! A.id authorId $ M.text $ foldMap (toUpper <<< take 1) clueAnswers
    H.div $ do
      H.label ! A.for charsId $ M.text "Letters remaining:"
      (renderCharCount $ lettersRemaining (countChars quote) clueAnswers) ! A.id charsId


clue :: forall e. Clue -> UI e Clue
clue x = { clue: _, answer: _ } <$> string "Clue:" x.clue
                                <*> string "Answer:" x.answer

acr :: forall e. UI e Markup
acr =
  renderAll <$> textarea "Quote:" "The only thing we have to fear is fear itself."
              <*> intSlider "Rows:" 1 10 4
              <*> resizableList "Clues:" clue { clue: "", answer: "" } defaultClues where
    defaultClues = L.fromFoldable $ { clue: "", answer: _ } <$> split (Pattern "") "franklinroosevelt"


main ∷ forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = do
  runFlareHTML "controls" "board" acr
