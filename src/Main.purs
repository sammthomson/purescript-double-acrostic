module Main where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array as Arr
import Data.Array (drop, length, take, uncons)
import Data.Foldable (class Foldable, foldMap, traverse_)
import Data.Group (ginverse)
import Data.List as L
import Data.List.Lazy as LL
import Data.Multiset as MS
import Data.String (singleton, toCharArray, toUpper)
import Data.String.Regex (Regex, replace, test)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Flare (UI, intSlider, resizableList, string, textarea)
import Flare.Smolder (runFlareHTML)
import Puzzle (Clue, Puzzle, answers, mkClue, mkPuzzle, source)
import Signal.Channel (CHANNEL)
import Text.Smolder.HTML as H
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!))
import Text.Smolder.Markup as M


type Markup = M.Markup Unit

countChars :: String -> MS.Multiset Char
countChars s = MS.fromFoldable $ toCharArray $ replace notLetters "" $ toUpper s

re :: String -> Regex
re s = unsafeRegex s global

punctuationStr :: String
punctuationStr = ",!@#$%^&*().:;-"

punctuation :: Regex
punctuation = re $ "[" <> punctuationStr <> "]"

lettersStr :: String
lettersStr = "A-Z"

notLetters :: Regex
notLetters = re $ "[^" <> lettersStr <> "]"

notDisplayableChar :: Regex
notDisplayableChar = re $ "[^ " <> lettersStr <> punctuationStr <> "]"

-- | Removes chars that shouldn't be displayed in the board.
cleanQuote :: String -> String
cleanQuote q = replace notDisplayableChar "" $ toUpper q

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
indexChars :: Array Char -> Array (Tuple Int Char)
indexChars chars =
  unfoldr step $ Tuple 1 chars where
    step (Tuple i xs) = incOnLetter <$> uncons xs where
      -- emit `(i, head)`, recurse on `(i+1, tail)`
      incOnLetter { head, tail } = Tuple (Tuple i head) (Tuple newI tail) where
        -- but only increment `i` if `head` is a letter
        newI = if test notLetters (singleton head) then i else i + 1


-- | Counts chars in `quote` that haven't been used yet in `clues`
lettersRemaining :: forall t. Functor t => Foldable t =>
                    String ->
                    t String ->
                    MS.Multiset Char
lettersRemaining quote clues =
  countChars quote <> ginverse (foldMap countChars clues)


-- | Renders a table with how many of each letter remain,
-- | with overused letters in red.
renderCharCount :: MS.Multiset Char -> Markup
renderCharCount cs =
  H.table $ traverse_ renderChar $ MS.entryList cs where
    renderChar (Tuple c i) = H.tr $ do
            td $ M.text $ singleton c <> ":"
            td $ M.text $ show i where
              td = if i < 0 then H.td ! A.className "err" else H.td


-- | Renders a single char and its index into the board.
renderCell :: Tuple Int Char -> Markup
renderCell (Tuple i c) =
  td $ do
    H.div ! A.className "idx" $ M.text $ show i
    H.div ! A.className "letter" $ M.text $ singleton c
    where td =
            if c == ' ' then
               H.td ! A.className "blank"
            else if test punctuation (singleton c) then
               H.td ! A.className "punct"
            else
               H.td


-- | Renders the board.
renderBoard :: String -> Int -> Markup
renderBoard quote numCols =
  H.table $ traverse_ renderRow $ formatQuote quote numCols where
    renderRow chars =
      H.tr $ traverse_ renderCell chars
    formatQuote quote numCols =
      reshape numCols $ indexChars (toCharArray $ cleanQuote quote)


renderPuzzle :: Puzzle -> Markup
renderPuzzle p =
  let
    charsLeft = lettersRemaining p.quote (answers p)
    boardId = "board"
    authorId = "author"
    charsId = "chars-left"
  in do
    H.div $ do
      (renderBoard p.quote p.numCols) ! A.id boardId
    H.div $ do
      H.label ! A.for authorId $ M.text "Source:"
      H.span ! A.id authorId $ M.text $ source p
    H.div $ do
      H.label ! A.for charsId $ M.text "Letters remaining:"
      (renderCharCount charsLeft) ! A.id charsId


clueUi :: forall e. Clue -> UI e Clue
clueUi clue = mkClue <$> string "Clue:" clue.clue
                     <*> string "Answer:" clue.answer

cluesUi :: forall e. L.List Clue -> UI e (L.List Clue)
cluesUi clues = resizableList "Clues:" clueUi emptyClue clues where
  emptyClue = mkClue "" ""


puzzleUi :: forall e. Puzzle -> UI e Puzzle
puzzleUi p = mkPuzzle <$> textarea "Quote:" p.quote
                      <*> intSlider "Columns:" 1 10 p.numCols
                      <*> cluesUi (L.fromFoldable p.clues)

acr :: forall e. UI e Markup
acr = renderPuzzle <$> puzzleUi defaultPuzzle where
    defaultPuzzle = {
      quote: "The only thing we have to fear is fear itself.",
      numCols: 12,
      clues: [
        mkClue "Snitch" "rat",
        mkClue "\"Lay _ me, I'm starving!\"" "off",
        mkClue "Common shower gift" "onesie",
        mkClue "Reason to stay home" "shy",
        mkClue "Ambivalent reply" "either",
        mkClue "Bland dessert, perhaps" "vegan",
        mkClue "Will Ferrell role" "elf",
        mkClue "Some soccer fields" "lit",
        mkClue "A thing to do in spring" "thaw"
      ]
    }


main ∷ forall e. Eff (dom :: DOM, channel :: CHANNEL | e) Unit
main = runFlareHTML "controls" "board" acr
