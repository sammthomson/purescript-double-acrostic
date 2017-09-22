module Play where

import Prelude (Unit, bind, const, discard, ($), (<$>), (>>=))

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId))
import Data.Array (foldMap, take, (!!))
import Data.Foldable (for_, traverse_)
import Data.Map (Map, empty, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (singleton, toCharArray)
import Text.Smolder.HTML (div, label, span, table, tr)
import Text.Smolder.HTML.Attributes (for, id)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.DOM (render)

import Edit (Cell(..), Html, indexChars, renderCell, reshape)
import Puzzle (Clue, Puzzle, answers, defaultPuzzle)


type Guess = Array (Maybe Char)

startClue :: Clue -> Guess
startClue c = const Nothing <$> toCharArray c.answer

data ClueCharIdx = ClueCharIdx Int Int  -- clue idx, char idx

type CharMap = Map Int ClueCharIdx  -- map from board index to clue char index

mkCharMap :: Puzzle -> CharMap
mkCharMap p = empty  -- TODO


type PuzzleInProgress = {
  solution :: Puzzle,
  guesses :: Array Guess,
  charMap :: CharMap
}

startPuzzle :: Puzzle -> PuzzleInProgress
startPuzzle p = {
  solution: p,
  guesses: startClue <$> p.clues,
  charMap: mkCharMap p
}

sourceGuess :: PuzzleInProgress -> Array (Maybe Char)
sourceGuess p = p.guesses >>= take 1

type BoardInProgress = Array Cell

board :: PuzzleInProgress -> BoardInProgress
board p = getCharFromClues <$> indexChars p.solution.quote where
  getCharFromClues (LetterCell i c) = LetterCell i $ fromMaybe ' ' newChar where
    newChar = do
      ClueCharIdx clueIdx charIdx <- lookup i p.charMap
      ans <- answers p.solution !! clueIdx
      toCharArray ans !! charIdx
  getCharFromClues c = c


-- | Renders the board.
renderBoard :: BoardInProgress -> Int -> Html
renderBoard b numCols =
  table $ traverse_ renderRow $ formattedQuote where
    renderRow row = tr $ traverse_ renderCell row
    formattedQuote = reshape numCols b


-- | Renders the board, source
renderPuzzle :: PuzzleInProgress -> Html
renderPuzzle p = do
  div $
    (renderBoard (board p) p.solution.numCols) ! id "board"
  div $ do
    label ! for "author" $ text "Source:"
    span ! id "author" $ text $ source where
      source = foldMap (\c -> singleton $ fromMaybe ' ' c) (sourceGuess p)


-- | More convenient `getElementById`
getElement :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElement name = do
  win <- window
  doc <- document win
  getElementById (ElementId name) (htmlDocumentToNonElementParentNode doc)


main ∷ forall e. Eff (dom :: DOM | e) Unit
main = do
  boardEl <- getElement "board"
  for_ boardEl \b -> render b (renderPuzzle $ startPuzzle defaultPuzzle)
