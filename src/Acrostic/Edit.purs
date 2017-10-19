module Acrostic.Edit (
  Cell(..),
  indexChars,
  main,
  renderCell,
  reshape
) where

import Prelude

import Acrostic.Puzzle (BoardIdx(..), CharType(..), Clue, Puzzle, cleanQuote, defaultPuzzle, lettersRemaining, mkClue, mkPuzzle, source, toJson)
import Acrostic.Gist (loadPuzzleFromGist)
import Acrostic.Puzzle (BoardIdx(..), CharType(..), Clue, Puzzle, cleanQuote, defaultPuzzle, lettersRemaining, mkClue, mkPuzzle, source)
import Acrostic.Gist (loadPuzzleFromGist)
import Acrostic.Puzzle (BoardIdx(..), CharType(..), Clue, Puzzle, cleanQuote, defaultPuzzle, lettersRemaining, mkClue, mkPuzzle, source, toJson)
import Control.Alt ((<|>))
import Acrostic.Gist (postPuzzleToGist)
import Acrostic.Puzzle (BoardIdx(..), CharType(..), Clue, Puzzle, cleanQuote, defaultPuzzle, lettersRemaining, mkClue, mkPuzzle, source)
import Acrostic.QueryString (setQueryStrings)
import Control.Lazy (defer)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (lift, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener)
import Data.Array (drop, length, take, uncons)
import Data.Array as Arr
import Data.Foldable (traverse_)
import Data.List as L
import Data.List.Lazy as LL
import Data.Multiset as MS
import Data.StrMap as StrMap
import Data.String (singleton)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Flare (ElementId, UI, intSlider, resizableList, runFlareWith, string, textarea)
import Flare.Custom (getElement)
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (button, div, label, span, table, td, tr)
import Text.Smolder.HTML.Attributes (className, for, id)
import Text.Smolder.Markup (Markup, (!), (#!), on, text)
import Text.Smolder.Renderer.DOM (patch)


-- | Renders a table with how many of each letter remain,
-- | with overused letters in red.
renderLetterCount :: forall e. MS.Multiset Char -> Markup e
renderLetterCount cs =
  table $ traverse_ renderChar $ MS.entryList cs where
    renderChar (Tuple c i) =
      tr $ do
        td' $ text $ singleton c <> ":"
        td' $ text $ show i where
          td' = if i >= 0 then td else td ! className "err"


-- only letters get indices
data Cell i = LetterCell i Char | PunctCell Char | SpaceCell


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
indexChars :: String -> Array (Cell BoardIdx)
indexChars quote =
  unfoldr maybeStep $ Tuple 0 $ cleanQuote quote where
    maybeStep (Tuple i xs) = step <$> uncons xs where
      step { head, tail } = Tuple cell (Tuple newI tail) where
        cell = case head of
          Letter c -> LetterCell (BoardIdx i) c
          Punct c -> PunctCell c
          _ -> SpaceCell
        -- only increment `i` if `head` is a letter
        newI = case head of
          Letter c -> i + 1
          _ -> i


-- | Renders a single char and its index into the board.
renderCell :: forall i e. Show i => Cell i -> Markup e
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
renderBoard :: forall e. String -> Int -> Markup e
renderBoard quote numCols =
  table $ traverse_ renderRow formattedQuote where
    renderRow row = tr $ traverse_ renderCell row
    formattedQuote = reshape numCols $ indexChars quote


-- | Renders the board, source, and table of remaining letters.
renderPuzzle :: forall e. Puzzle -> Markup (EventListener (dom :: DOM, ajax :: AJAX | e))
renderPuzzle p =
  let
    save e = launchAff_ $ runExceptT $ do
      id <- postPuzzleToGist p
      -- todo show instructions to boorkmark, also show errors
      liftEff $ setQueryStrings (StrMap.singleton "gist" id)
  in
    do
      div $
        button #! on "click" (eventListener save) $ text "Save"
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
puzzleUi p = mkPuzzle <$> string "Title:" p.title
                      <*> textarea "Quote:" p.quote
                      <*> intSlider "Columns:" 1 20 p.numCols
                      <*> cluesUi (L.fromFoldable p.clues)


type DC e = (dom :: DOM, channel :: CHANNEL | e)
-- | Renders a Flare UI with `Markup` as output. The first ID specifies
-- | the DOM element for the controls while the second ID specifies the
-- | element for the output.
runFlareDom :: forall e.
               ElementId
               -> ElementId
               -> UI e (Markup (EventListener (DC e)))
               -> Eff (DC e) Unit
runFlareDom controlsId targetId =
  runFlareWith controlsId handler where
    handler markup = void $ runMaybeT do
      target <- MaybeT (getElement targetId)
      lift $ patch target markup

main ∷ forall e. Eff (dom :: DOM,
                      channel :: CHANNEL,
                      ajax :: AJAX | e) Unit
main = launchAff_ $ do
  -- todo: load puzzle if queryparam is present
  let htmlUi = renderPuzzle <$> puzzleUi defaultPuzzle
  liftEff (runFlareDom "controls" "board" htmlUi)
