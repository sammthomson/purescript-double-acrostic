module Acrostic.Play where

import Acrostic.Edit (Cell(..), Html, indexChars, renderCell, reshape)
import Acrostic.Puzzle (
  BoardIdx(..), CharIdx(..), CharMap, Clue, ClueCharIdx(..), ClueIdx(..),
  Puzzle, answers, defaultPuzzle)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId))
import Data.Array (length, mapMaybe, take, zip, zipWith, (!!), (..))
import Data.Foldable (foldl, for_, sequence_, traverse_)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (singleton, toCharArray, toUpper)
import Data.Tuple (Tuple(..), fst)
import Prelude (
  Unit, bind, const, discard, ($), (<$>), (<*>), (<<<), (<>), (>>=))
import Text.Smolder.HTML (div, label, span, table, td, tr)
import Text.Smolder.HTML.Attributes (id)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.DOM (render)


main ∷ forall e. Eff (dom :: DOM | e) Unit
main = do
  boardEl <- getElement "board"
  for_ boardEl \b -> render b (renderPuzzle $ startPuzzle defaultPuzzle)


type PuzzleInProgress = {
  solution :: Puzzle,
  guesses :: Array Guess,
  charMap :: CharMap
}

type Guess = Array (Maybe Char)


startPuzzle :: Puzzle -> PuzzleInProgress
startPuzzle p = {
  solution: p,
  guesses: startGuess <$> p.clues,
  charMap: mkCharMap p
}

startGuess :: Clue -> Guess
startGuess c = const Nothing <$> toCharArray c.answer


zipWithIndex :: forall a. Array a -> Array (Tuple Int a)
zipWithIndex xs = zip (0..(length xs)) xs


groupCharsByIdx :: Array String -> M.Map Char (L.List (Tuple Int Int))
groupCharsByIdx clues = foldl step M.empty flattened where
  idxd = zipWithIndex ((zipWithIndex <<< toCharArray <<< toUpper) <$> clues)
  flattened = idxd >>= \(Tuple i xs) -> Tuple <$> [i] <*> xs
  step acc (Tuple clueI (Tuple charI c)) =
    M.unionWith (<>) acc $ M.singleton c v where
      v = L.singleton $ Tuple clueI charI


-- TODO: should this be randomized?
mkCharMap :: Puzzle -> CharMap
mkCharMap p = fst result where
  result = foldl step start (indexChars p.quote)
  start = Tuple M.empty (groupCharsByIdx (answers p))
  step (Tuple cMap rem) (LetterCell (BoardIdx i) c)
      -- these are pattern guards. this case of `step` only applies if they succeed
      | Just idxs <- M.lookup c rem,
        Just { head: Tuple clueIdx charIdx, tail: tl } <- L.uncons idxs =
    Tuple (M.insert boardIdx newAssoc cMap) (M.insert c tl rem) where
      boardIdx = BoardIdx i
      newAssoc = ClueCharIdx (ClueIdx clueIdx) (CharIdx charIdx) boardIdx
  step acc _ = acc


source :: PuzzleInProgress -> Array (Maybe Char)
source p = p.guesses >>= take 1


type CellInProgress = Cell ClueCharIdx

type BoardInProgress = Array CellInProgress

lookupChar :: ClueCharIdx -> PuzzleInProgress -> Maybe Char
lookupChar (ClueCharIdx (ClueIdx clueIdx) (CharIdx charIdx) _) p = do
  guess <- p.guesses !! clueIdx
  maybeChar <- guess !! charIdx
  maybeChar


board :: PuzzleInProgress -> BoardInProgress
board p = mapMaybe getCharFromClues $ indexChars p.solution.quote where
  getCharFromClues (LetterCell i c) = do
    newIdx <- M.lookup i p.charMap  -- this should never be Nothing
    let newChar = lookupChar newIdx p
    Just $ LetterCell newIdx (fromMaybe ' ' newChar)
  getCharFromClues (PunctCell c) = Just $ PunctCell c
  getCharFromClues SpaceCell = Just SpaceCell


-- | Renders the board.
renderBoard :: BoardInProgress -> Int -> Html
renderBoard b numCols =
  table $ traverse_ renderRow $ formattedQuote where
    renderRow row = tr $ traverse_ renderCell row
    formattedQuote = reshape numCols b


renderMaybeChar :: Maybe Char -> Html
renderMaybeChar mc = td $ text $ singleton $ fromMaybe ' ' mc

renderGuess :: String -> Array (Maybe Char) -> Html
renderGuess clue guess =
  div $ do
    label $ text clue
    span $ table $ tr $ traverse_ renderMaybeChar guess

-- | Renders the board, source, and clues
renderPuzzle :: PuzzleInProgress -> Html
renderPuzzle p = do
  div $
    (renderBoard (board p) p.solution.numCols) ! id "board"
  div $ renderGuess "Source:" (source p) ! id "author"
  div $
    (sequence_ $ zipWith renderGuess (_.clue <$> p.solution.clues) p.guesses) ! id "guesses"


-- | More convenient `getElementById`
getElement :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElement name = do
  win <- window
  doc <- document win
  getElementById (ElementId name) (htmlDocumentToNonElementParentNode doc)
