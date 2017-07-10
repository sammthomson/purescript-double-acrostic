module Main where

import Prelude

import Control.Lazy as Z
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.Array as Arr
import Data.Dictionary (Dictionary(..), defaultPath, loadDict)
import Data.Foldable (class Foldable, traverse_)
import Data.Int (ceil, toNumber)
import Data.List.Lazy as LL
import Data.Multiset as MS
import Data.Set as S
import Data.String (Pattern(..), Replacement(..), replaceAll, singleton, toCharArray, toUpper)
import Data.String.Regex as R
import Data.String.Regex.Unsafe as R
import Data.String.Regex.Flags as R
import Data.Tuple
import Flare (UI, int, intSlider, intSlider_, string, textarea)
import Flare.Smolder (runFlareHTML)
import Math as Math
import Node.FS (FS)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup as M


type Markup = M.Markup Unit


countChars :: String -> MS.Multiset Char
countChars = toUpper >>> toCharArray >>> (Arr.filter ((/=) ' ')) >>> MS.fromFoldable


cell :: Tuple Int Char -> Markup
cell (Tuple i c) =
  td $ do
    H.div ! A.className "idx" $ M.text $ show (i + 1)
    H.div ! A.className "letter" $ M.text $ singleton c
    where td = if c == ' ' then H.td ! A.className "blank" else H.td

notLetterOrSpace :: R.Regex
notLetterOrSpace = R.unsafeRegex "[^ A-Z]" R.global

cleanQuote :: String -> String
cleanQuote q = R.replace notLetterOrSpace "" $ toUpper q

formatQuote :: String -> Int -> Array (Array (Tuple Int Char))
formatQuote quote numRows =
  Arr.fromFoldable $ LL.take numRows $ rows (Arr.mapWithIndex Tuple chars)
  where
    chars = toCharArray $ cleanQuote quote
    numCols = ceil $ (toNumber $ Arr.length chars) / (toNumber numRows)
    rows cs = LL.cons (Arr.take numCols cs) (Z.defer \_ ->
                                              rows $ Arr.drop numCols cs)

renderBoard :: String -> String -> Int -> Markup
renderBoard quote author numRows =
  H.table $ do
    traverse_ row $ formatQuote quote numRows
      where row chars = H.tr $ traverse_ cell chars


-- main :: forall e. Eff (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | e) Unit
-- main = do
--   Dictionary dict <- loadDict defaultPath
--   pure unit
--   log (show (LL.take 2 (S.toUnfoldable dict)))
  -- log $ toUpper example.quote
  -- log ("Letters: " <> show (countChars example.quote)) where
  --   example =
  --     { quote: "The only thing we have to fear is fear itself."
  --     , author: "Franklin Roosevelt"
  --     }


acr :: forall e. UI e Markup
acr =
  renderBoard <$> textarea "Quote:" "The only thing we have to fear is fear itself."
              <*> string "Author:" "Franklin Roosevelt"
              <*> intSlider "Rows:" 1 10 4


main ∷ forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = do
  runFlareHTML "controls" "board" acr
