module Main where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array as Arr
import Data.Foldable (foldr, traverse_)
import Data.Int (ceil, toNumber)
import Data.List.Lazy as LL
import Data.Multiset as MS
import Data.String (singleton, toCharArray, toUpper)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Flare (UI, intSlider, string, textarea)
import Flare.Smolder (runFlareHTML)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup as M


type Markup = M.Markup Unit


countChars :: String -> MS.Multiset Char
countChars = cleanQuote >>> toCharArray >>> (Arr.filter ((/=) ' ')) >>> MS.fromFoldable


cell :: Tuple Int Char -> Markup
cell (Tuple i c) =
  td $ do
    H.div ! A.className "idx" $ M.text $ show (i + 1)
    H.div ! A.className "letter" $ M.text $ singleton c
    where td = if c == ' ' then H.td ! A.className "blank" else H.td


cleanQuote :: String -> String
cleanQuote q = replace notLetterOrSpace "" $ toUpper q
  where notLetterOrSpace = unsafeRegex "[^ A-Z]" global


formatQuote :: String -> Int -> Array (Array (Tuple Int Char))
formatQuote quote numRows =
  Arr.fromFoldable $ LL.take numRows $ rows (Arr.mapWithIndex Tuple chars)
  where
    chars = toCharArray $ cleanQuote quote
    numCols = ceil $ (toNumber $ Arr.length chars) / (toNumber numRows)
    rows cs = LL.cons (Arr.take numCols cs) (defer \_ ->
                                              rows $ Arr.drop numCols cs)

lettersRemaining :: MS.Multiset Char -> String -> MS.Multiset Char
lettersRemaining quoteChars clue =
  foldr MS.delete quoteChars (toCharArray $ toUpper $ clue)


renderCharCount :: MS.Multiset Char -> Markup
renderCharCount cs =
  H.table $ traverse_ renderChar $ MS.toUnfoldable cs :: LL.List _ where
    renderChar (Tuple c i) = H.tr $ do
            td $ M.text $ singleton c
            td $ M.text $ show i where
              td = if i < 0 then H.td ! A.className "err" else H.td


renderBoard :: String -> Int -> Markup
renderBoard quote numRows =
  H.table $ traverse_ row $ formatQuote quote numRows
    where row chars = H.tr $ traverse_ cell chars


renderAll :: String -> String -> Int -> String -> Markup
renderAll quote author numRows clue = do
  H.h3 $ M.text "Board:"
  renderBoard quote numRows
  H.h3 $ M.text "Letters remaining:"
  renderCharCount $ lettersRemaining (countChars quote) clue


acr :: forall e. UI e Markup
acr =
  renderAll <$> textarea "Quote:" "The only thing we have to fear is fear itself."
              <*> string "Author:" "Franklin Roosevelt"
              <*> intSlider "Rows:" 1 10 4
              <*> textarea "Clue:" ""


main ∷ forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = do
  runFlareHTML "controls" "board" acr
