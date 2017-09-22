module Play where

import Prelude (($), Unit, bind, discard)

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId))
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Edit (Html, renderBoard)
import Puzzle (Puzzle, defaultPuzzle, source)
import Text.Smolder.HTML (div, label, span)
import Text.Smolder.HTML.Attributes (for, id)
import Text.Smolder.Markup (text, (!))
import Text.Smolder.Renderer.DOM (render)


-- | Renders the board, source
renderPuzzle :: Puzzle -> Html
renderPuzzle p = do
  div $
    (renderBoard p.quote p.numCols) ! id "board"
  div $ do
    label ! for "author" $ text "Source:"
    span ! id "author" $ text $ source p


-- | More convenient `getElementById`
getElement :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElement name = do
  win <- window
  doc <- document win
  getElementById (ElementId name) (htmlDocumentToNonElementParentNode doc)


main ∷ forall e. Eff (dom :: DOM | e) Unit
main = do
  boardEl <- getElement "board"
  for_ boardEl \b -> render b (renderPuzzle defaultPuzzle)
