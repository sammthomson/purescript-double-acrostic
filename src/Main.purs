module Main where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array as Arr
import Data.Array (drop, length, take, uncons)
import Data.Foldable (traverse_)
import Data.List as L
import Data.List.Lazy as LL
import Data.Multiset as MS
import Data.String (singleton)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Flare (UI, intSlider, resizableList, string, textarea)
import Flare.Smolder (runFlareHTML)
import Puzzle (CharType(..), Clue, Puzzle, cleanQuote, defaultPuzzle,
               lettersRemaining, mkClue, mkPuzzle, source)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup as M


type Markup = M.Markup Unit


-- | Renders a table with how many of each letter remain,
-- | with overused letters in red.
renderLetterCount :: MS.Multiset Char -> Markup
renderLetterCount cs =
  H.table $ traverse_ renderChar $ MS.entryList cs where
    renderChar (Tuple c i) = H.tr $ do
            td $ M.text $ singleton c <> ":"
            td $ M.text $ show i where
              td = if i < 0 then H.td ! A.className "err" else H.td


-- only letters get indices
data Cell = LetterCell Int Char | PunctCell Char | SpaceCell


-- | Arranges `xs` into rows of length `numCols` (the last row might
-- | be shorter).
reshape :: forall a. Int -> Array a -> Array (Array a)
reshape numCols xs =
  Arr.fromFoldable $ LL.takeWhile (\row -> length row > 0) $ chunk xs where
    -- keep lazily taking `numCols` elements forever
    chunk ys = LL.cons hd $ defer \_ -> chunk tl
      where
        hd = take numCols ys
        tl = drop numCols ys


-- | Pairs each letter with its index (1-indexed).
indexChars :: Array CharType -> Array Cell
indexChars chars =
  unfoldr step $ Tuple 1 chars where
    step (Tuple i xs) = incOnLetter <$> uncons xs where
      incOnLetter { head, tail } = Tuple (cell head) (Tuple (newI head) tail) where
        cell (Letter c) = LetterCell i c
        cell (Punct c) = PunctCell c
        cell _ = SpaceCell
        -- but only increment `i` if `head` is a letter
        newI (Letter c) = i + 1
        newI _ = i


-- | Renders a single char and its index into the board.
renderCell :: Cell -> Markup
renderCell (LetterCell i c) =
  H.td $ do
    H.div ! A.className "idx" $ M.text $ show i
    H.div ! A.className "letter" $ M.text $ singleton c
renderCell (PunctCell c) =
  H.td ! A.className "punct" $
    H.div ! A.className "letter" $ M.text $ singleton c
renderCell (SpaceCell) =
  H.td ! A.className "blank" $
    H.div ! A.className "letter" $ M.text $ ""


-- | Renders the board.
renderBoard :: String -> Int -> Markup
renderBoard quote numCols =
  H.table $ traverse_ renderRow formattedQuote where
    renderRow row = H.tr $ traverse_ renderCell row
    formattedQuote = reshape numCols $ indexChars $ cleanQuote quote


renderPuzzle :: Puzzle -> Markup
renderPuzzle p = do
  H.div $
    (renderBoard p.quote p.numCols) ! A.id "board"
  H.div $ do
    H.label ! A.for "author" $ M.text "Source:"
    H.span ! A.id "author" $ M.text $ source p
  H.div $ do
    H.label ! A.for "chars-left" $ M.text "Letters remaining:"
    (renderLetterCount $ lettersRemaining p) ! A.id "chars-left"


clueUi :: forall e. Clue -> UI e Clue
clueUi clue = mkClue <$> string "Clue:" clue.clue
                     <*> string "Answer:" clue.answer

cluesUi :: forall e. L.List Clue -> UI e (L.List Clue)
cluesUi clues = resizableList "Clues:" clueUi emptyClue clues where
  emptyClue = mkClue "" ""


puzzleUi :: forall e. Puzzle -> UI e Puzzle
puzzleUi p = mkPuzzle <$> textarea "Quote:" p.quote
                      <*> intSlider "Columns:" 1 20 p.numCols
                      <*> cluesUi (L.fromFoldable p.clues)


main ∷ forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = runFlareHTML "controls" "board" (renderPuzzle <$> puzzleUi defaultPuzzle)
