module Acrostic.Play where

import Acrostic.Edit (Cell(..), Html, indexChars, renderCell, reshape)
import Acrostic.Puzzle (
  BoardIdx(..), CharIdx(..), CharMap, Clue, ClueCharBoardIdx(..),
  ClueCharIdx(..), ClueIdx(..), Puzzle, answers, defaultPuzzle)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId))
import Data.Array (head, length, mapMaybe, zip, zipWith, (!!), (..))
import Data.Bimap as B
import Data.Foldable (foldl, for_, sequence_, traverse_)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toCharArray, toUpper)
import Data.Tuple (Tuple(..), fst)
import Prelude (
  Unit, bind, const, discard, join, pure, ($), (<$>), (<#>), (<<<), (<>))
import Text.Smolder.HTML (div, label, span, table, tr)
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


indices :: forall a. Array a -> Array Int
indices xs = 0..(length xs)

zipWithIndex :: forall a. Array a -> Array (Tuple Int a)
zipWithIndex xs = zip (indices xs) xs


indexClueChars :: forall a.
                Array (Array a) ->
                Array (Array (Tuple ClueCharIdx a))
indexClueChars guesses =
  let
    idxd = zipWithIndex (zipWithIndex <$> guesses)
  in
    idxd <#> \(Tuple clueIdx guess) ->
      guess <#> \(Tuple charIdx char) ->
        Tuple (ClueCharIdx (ClueIdx clueIdx) (CharIdx charIdx)) char

indexedGuesses :: PuzzleInProgress ->
                  Array (Array (Tuple ClueCharBoardIdx (Maybe Char)))
indexedGuesses p = mapMaybe addBoardIdx <$> indexClueChars p.guesses where
  invCharMap = B.invert p.charMap
  addBoardIdx (Tuple cci c) = do
    boardIdx <- B.lookup cci invCharMap
    Just $ Tuple (ClueCharBoardIdx cci boardIdx) c

groupCharsByIdx :: Array String -> M.Map Char (L.List ClueCharIdx)
groupCharsByIdx clues = foldl step M.empty idxd where
  idxd = join $ indexClueChars ((toCharArray <<< toUpper) <$> clues)
  step acc (Tuple cci c) =
    M.unionWith (<>) acc $ M.singleton c (L.singleton cci)


-- TODO: should this be randomized?
mkCharMap :: Puzzle -> CharMap
mkCharMap p = fst result where
  result = foldl step start (indexChars p.quote)
  start = Tuple B.empty (groupCharsByIdx (answers p))
  step (Tuple cMap rem) (LetterCell (BoardIdx i) c)
      -- these are pattern guards.
      -- this case of `step` only applies if they succeed
      | Just idxs <- M.lookup c rem,
        Just { head: cci, tail: tl } <- L.uncons idxs =
    Tuple (B.insert (BoardIdx i) cci cMap) (M.insert c tl rem)
  step acc _ = acc


sourceGuess :: PuzzleInProgress -> Array (Tuple ClueCharBoardIdx (Maybe Char))
sourceGuess p = do
  let invCharMap = B.invert p.charMap
  Tuple i guess <- zipWithIndex p.guesses
  let clueCharIdx = ClueCharIdx (ClueIdx i) (CharIdx 0)
  let boardIdx = fromMaybe (BoardIdx 0) $ B.lookup clueCharIdx invCharMap
  pure $ Tuple (ClueCharBoardIdx clueCharIdx boardIdx) (join $ head guess)


type CellInProgress = Cell ClueCharBoardIdx

type BoardInProgress = Array CellInProgress

lookupChar :: ClueCharIdx -> PuzzleInProgress -> Maybe Char
lookupChar (ClueCharIdx (ClueIdx clueIdx) (CharIdx charIdx)) p = do
  guess <- p.guesses !! clueIdx
  maybeChar <- guess !! charIdx
  maybeChar


board :: PuzzleInProgress -> BoardInProgress
board p = mapMaybe getCharFromClues $ indexChars p.solution.quote where
  getCharFromClues (LetterCell boardIdx c) = do
    clueCharIdx <- B.lookup boardIdx p.charMap
    let char = lookupChar clueCharIdx p
    let idx = ClueCharBoardIdx clueCharIdx boardIdx
    Just $ LetterCell idx (fromMaybe ' ' char)
  getCharFromClues (PunctCell c) = Just $ PunctCell c
  getCharFromClues SpaceCell = Just SpaceCell


-- | Renders the board.
renderBoard :: BoardInProgress -> Int -> Html
renderBoard b numCols =
  table $ traverse_ renderRow formattedQuote where
    renderRow row = tr $ traverse_ renderCell row
    formattedQuote = reshape numCols b


renderMaybeChar :: Tuple ClueCharBoardIdx (Maybe Char) -> Html
renderMaybeChar (Tuple i mc) = renderCell $ LetterCell i $ fromMaybe ' ' mc

renderGuess :: String -> Array (Tuple ClueCharBoardIdx (Maybe Char)) -> Html
renderGuess clue guess =
  div $ do
    label $ text clue
    span $ table $ tr $ traverse_ renderMaybeChar guess

-- | Renders the board, source, and clues
renderPuzzle :: PuzzleInProgress -> Html
renderPuzzle p = do
  div $
    (renderBoard (board p) p.solution.numCols) ! id "board"
  div $ renderGuess "source:" (sourceGuess p) ! id "author"
  div $ do
    let clues = _.clue <$> p.solution.clues
    (sequence_ $ zipWith renderGuess clues (indexedGuesses p)) ! id "guesses"


-- | More convenient `getElementById`
getElement :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElement name = do
  win <- window
  doc <- document win
  getElementById (ElementId name) (htmlDocumentToNonElementParentNode doc)
