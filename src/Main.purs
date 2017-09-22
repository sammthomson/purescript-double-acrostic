module Main where

import Prelude (($), (<$>), (<*>), (<>), (>=), (>), (+), Unit, discard, show)

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
import Text.Smolder.HTML (div, label, span, table, td, tr)
import Text.Smolder.HTML.Attributes (className, for, id)
import Text.Smolder.Markup (Markup, (!), text)


-- | HTML without any event handlers
type PureMarkup = Markup Unit


-- | Renders a table with how many of each letter remain,
-- | with overused letters in red.
renderLetterCount :: MS.Multiset Char -> PureMarkup
renderLetterCount cs =
  table $ traverse_ renderChar $ MS.entryList cs where
    renderChar (Tuple c i) =
      tr $ do
        td' $ text $ singleton c <> ":"
        td' $ text $ show i where
          td' = if i >= 0 then td else td ! className "err"


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
  unfoldr maybeStep $ Tuple 1 chars where
    maybeStep (Tuple i xs) = step <$> uncons xs where
      step { head, tail } = Tuple cell (Tuple newI tail) where
        cell = case head of
          Letter c -> LetterCell i c
          Punct c -> PunctCell c
          _ -> SpaceCell
        -- only increment `i` if `head` is a letter
        newI = case head of
          Letter c -> i + 1
          _ -> i


-- | Renders a single char and its index into the board.
renderCell :: Cell -> PureMarkup
renderCell (LetterCell i c) =
  td $ do
    div ! className "idx" $ text $ show i
    div ! className "letter" $ text $ singleton c
renderCell (PunctCell c) =
  td ! className "punct" $
    div ! className "letter" $ text $ singleton c
renderCell (SpaceCell) =
  td ! className "blank" $
    div ! className "letter" $ text $ ""


-- | Renders the board.
renderBoard :: String -> Int -> PureMarkup
renderBoard quote numCols =
  table $ traverse_ renderRow formattedQuote where
    renderRow row = tr $ traverse_ renderCell row
    formattedQuote = reshape numCols $ indexChars $ cleanQuote quote


-- | Renders the board, source, and table of remaining letters.
renderPuzzle :: Puzzle -> PureMarkup
renderPuzzle p = do
  div $
    (renderBoard p.quote p.numCols) ! id "board"
  div $ do
    label ! for "author" $ text "Source:"
    span ! id "author" $ text $ source p
  div $ do
    label ! for "chars-left" $ text "Letters remaining:"
    (renderLetterCount $ lettersRemaining p) ! id "chars-left"


-- | UI for a single clue
clueUi :: forall e. Clue -> UI e Clue
clueUi clue = mkClue <$> string "Clue:" clue.clue
                     <*> string "Answer:" clue.answer

-- | UI for a list of clues
cluesUi :: forall e. L.List Clue -> UI e (L.List Clue)
cluesUi clues = resizableList "Clues:" clueUi emptyClue clues where
  emptyClue = mkClue "" ""


-- | UI for the entire puzzle
puzzleUi :: forall e. Puzzle -> UI e Puzzle
puzzleUi p = mkPuzzle <$> textarea "Quote:" p.quote
                      <*> intSlider "Columns:" 1 20 p.numCols
                      <*> cluesUi (L.fromFoldable p.clues)


main ∷ forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = runFlareHTML "controls" "board" $
        renderPuzzle <$> puzzleUi defaultPuzzle
