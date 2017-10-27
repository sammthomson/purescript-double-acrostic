module Acrostic.Play where

import Prelude hiding (div, id)

import Acrostic.Edit (Cell(..), indexChars, renderCell, reshape)
import Acrostic.Gist (loadPuzzleFromQueryString)
import Acrostic.Puzzle (BoardIdx(..), CharIdx(..), CharMap, Clue, ClueCharBoardIdx(..), ClueCharIdx(..), ClueIdx(..), Puzzle, answers, clues)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExceptT)
import DOM (DOM)
import Data.Array (head, length, mapMaybe, zip, zipWith, (!!), (..))
import Data.Bimap as B
import Data.Foldable (foldl, traverse_)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toCharArray, toUpper)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Flare (UI, fieldset)
import Flare.Custom (upperChar, rowUi)
import Flare.Smolder (runFlareHTML)
import Network.HTTP.Affjax (AJAX)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML (div, label, span, table, tr)
import Text.Smolder.HTML.Attributes (id, for)
import Text.Smolder.Markup (Markup, text, (!))


main :: forall e. Eff (dom :: DOM,
                       console :: CONSOLE,
                       channel :: CHANNEL,
                       ajax :: AJAX | e) Unit
main = launchAff_ $ runExceptT do
  puzz <- loadPuzzleFromQueryString
  liftEff $
    runFlareHTML "controls" "board" $
      renderPuzzle <$> puzzleUi (startPuzzle puzz)


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

 -- We'll be annotating all of these chars with both where they live in the
 -- clue guesses and where they live in the board.
type IdxdChar = Tuple ClueCharBoardIdx (Maybe Char)
type IdxdGuess = Array IdxdChar
data IdxdClueAndGuess = IdxdClueAndGuess ClueIdx String IdxdGuess

indexClueChars :: forall a.
                Array (Array a) ->
                Array (Array (Tuple ClueCharIdx a))
indexClueChars guesses =
  let
    charIdxd = zipWithIndex <$> guesses
    clueIdxd = zipWithIndex charIdxd
  in
    clueIdxd <#> \(Tuple clueIdx guess) ->
      guess <#> \(Tuple charIdx char) ->
        Tuple (ClueCharIdx (ClueIdx clueIdx) (CharIdx charIdx)) char

indexedGuesses :: PuzzleInProgress -> Array IdxdClueAndGuess
indexedGuesses p = f <$> (zipWithIndex $ zip (clues p.solution) idxdGuesses) where
  f (Tuple clueIdx (Tuple clue guess)) = IdxdClueAndGuess (ClueIdx clueIdx) clue guess
  idxdGuesses :: Array IdxdGuess
  idxdGuesses = mapMaybe addBoardIdx <$> indexClueChars p.guesses
  invCharMap = B.invert p.charMap
  addBoardIdx (Tuple cci c) = do
    boardIdx <- B.lookup cci invCharMap
    Just $ Tuple (ClueCharBoardIdx cci boardIdx) c

-- | Builds a map from char to a list of indices where it appears in clue
-- | guesses.
groupCharsByIdx :: Array String -> M.Map Char (L.List ClueCharIdx)
groupCharsByIdx clues = foldl step M.empty idxd where
  idxd = join $ indexClueChars ((toCharArray <<< toUpper) <$> clues)
  step acc (Tuple cci c) =
    M.unionWith (<>) acc $ M.singleton c (L.singleton cci)

-- | Builds a bimap from clue char indices to board indices.
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


-- | Anagram of the clue guesses
sourceGuess :: PuzzleInProgress -> IdxdGuess
sourceGuess p =
  mapMaybe (\(IdxdClueAndGuess _ _ guess) -> head guess) (indexedGuesses p)


type CellInProgress = Cell ClueCharBoardIdx

type BoardInProgress = Array CellInProgress

lookupChar :: ClueCharIdx -> PuzzleInProgress -> Maybe Char
lookupChar (ClueCharIdx (ClueIdx clueIdx) (CharIdx charIdx)) p = do
  guess <- p.guesses !! clueIdx
  mc <- guess !! charIdx
  mc


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
renderBoard :: forall e. BoardInProgress -> Int -> Markup e
renderBoard b numCols =
  table $ traverse_ renderRow formattedQuote where
    renderRow row = tr $ traverse_ renderCell row
    formattedQuote = reshape numCols b


idxdCharUi :: forall e. IdxdChar -> UI e IdxdChar
idxdCharUi (Tuple idx mc) = Tuple idx <$> upperChar (show idx) mc

guessUi :: forall e. IdxdClueAndGuess -> UI e Guess
guessUi (IdxdClueAndGuess clueIdx clue guess) =
  fieldset title $ (map snd <$> rowUi idxdCharUi guess) where
    title = show clueIdx <> ". " <> clue

puzzleUi :: forall e. PuzzleInProgress -> UI e PuzzleInProgress
puzzleUi p = setGuesses <$> guessesUi (indexedGuesses p) where
  guessesUi = traverse guessUi
  setGuesses gs = p { guesses = gs }

-- | Renders the board, source, and clues
renderPuzzle :: forall e. PuzzleInProgress -> Markup e
renderPuzzle p = let
    renderSource guess = span $ table $ tr $ traverse_ renderMaybeChar guess
    renderMaybeChar (Tuple i mc) = renderCell $ LetterCell i $ fromMaybe ' ' mc
  in
    do
      div $ do
        label ! for "puzzle-title" $ text "Title:"
        span ! id "puzzle-title" $ text $ p.solution.title
      div $
        (renderBoard (board p) p.solution.numCols) ! id "board"
      div $ do
        label ! for "author" $ text "Source:"
        renderSource (sourceGuess p) ! id "author"

-- utils --
indices :: forall a. Array a -> Array Int
indices xs = 0..(length xs)

zipWithIndex :: forall a. Array a -> Array (Tuple Int a)
zipWithIndex xs = zipWith Tuple (indices xs) xs
