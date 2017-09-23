module Acrostic.Play where

import Acrostic.Edit (Cell(..), Html, indexChars, renderCell, reshape)
import Acrostic.Puzzle (Clue, Puzzle, answers, defaultPuzzle)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId))
import Data.Array (length, mapMaybe, take, zip, zipWith, (!!), (..))
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (foldl, for_, sequence_, traverse_)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String (singleton, toCharArray, toUpper)
import Data.Tuple (Tuple(..), fst)
import Prelude (class Eq, class Ord, class Show, Unit, bind, const, discard, show, ($), (<<<), (+), (<$>), (<*>), (<>), (>>=))
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

-- | A map from board index to clue char index
type CharMap = M.Map Int ClueCharIdx

data ClueCharIdx = ClueCharIdx ClueIdx Int  -- clue idx, char idx

newtype ClueIdx = ClueIdx Int


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


groupCharsByIdx :: Array String -> M.Map Char (L.List ClueCharIdx)
groupCharsByIdx clues = foldl step M.empty flattened where
  idxd = zipWithIndex ((zipWithIndex <<< toCharArray <<< toUpper) <$> clues)
  flattened = idxd >>= \(Tuple i xs) -> Tuple <$> [i] <*> xs
  step acc (Tuple clueI (Tuple charI c)) =
    M.unionWith (<>) acc $ M.singleton c v where
      v = L.singleton $ ClueCharIdx (ClueIdx clueI) charI


mkCharMap :: Puzzle -> CharMap
mkCharMap p = fst result where
  result = foldl step start (indexChars p.quote)
  start = Tuple M.empty (groupCharsByIdx (answers p))
  step (Tuple cMap rem) (LetterCell i c)
      | Just idxs <- M.lookup c rem,
        Just { head, tail } <- L.uncons idxs =  -- TODO: this should be randomized
    Tuple (M.insert i head cMap) (M.insert c tail rem)
  step acc _ = acc


source :: PuzzleInProgress -> Array (Maybe Char)
source p = p.guesses >>= take 1


type CellInProgress = Cell ClueCharIdx

type BoardInProgress = Array CellInProgress

lookupChar :: ClueCharIdx -> PuzzleInProgress -> Maybe Char
lookupChar (ClueCharIdx (ClueIdx clueIdx) charIdx) p = do
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

-- | Renders the board, source
renderPuzzle :: PuzzleInProgress -> Html
renderPuzzle p = do
  div $
    (renderBoard (board p) p.solution.numCols) ! id "board"
  div $ renderGuess "Source:" (source p)
  div $
    (sequence_ $ zipWith renderGuess (_.clue <$> p.solution.clues) p.guesses) ! id "guesses"


-- | More convenient `getElementById`
getElement :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElement name = do
  win <- window
  doc <- document win
  getElementById (ElementId name) (htmlDocumentToNonElementParentNode doc)


derive instance newtypeClueIdx :: Newtype ClueIdx _
derive instance eqClueIdx :: Eq ClueIdx
derive instance ordClueIdx :: Ord ClueIdx

instance showClueIdx :: Show ClueIdx where
  show (ClueIdx i) = singleton $ fromCharCode $ toCharCode 'A' + i


instance showClueCharIdx :: Show ClueCharIdx where
  show (ClueCharIdx clueIdx charIdx) = show clueIdx <> " " <> show charIdx

derive instance eqClueCharIdx :: Eq ClueCharIdx
derive instance ordClueCharIdx :: Ord ClueCharIdx
