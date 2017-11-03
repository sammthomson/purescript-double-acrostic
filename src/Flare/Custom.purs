module Flare.Custom (
  clobberRender,
  getElement,
  nextInput,
  previousInput,
  removeAllChildren,
  rowUi,
  upperChar
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.List.Trans as LT
import DOM (DOM)
import DOM.Event.EventTarget (EventListener)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Element (tagName)
import DOM.Node.Node (childNodes, parentElement, removeChild)
import DOM.Node.NodeList (toArray)
import DOM.Node.NonDocumentTypeChildNode (nextElementSibling, previousElementSibling)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.ParentNode (firstElementChild, lastElementChild)
import DOM.Node.Types (Element, ElementId(..), Node, elementToNode, elementToNonDocumentTypeChildNode, elementToParentNode)
import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (singleton, toCharArray, toLower)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Flare (Flare, Label, UI, setupFlare)
import Signal as S
import Signal.Channel (CHANNEL, subscribe, send, channel)
import Text.Smolder.Markup (Markup)
import Text.Smolder.Renderer.DOM (render)
import Type.Data.Boolean (kind Boolean)


foreign import cUpperChar :: forall e.
                             (Element -> Eff (dom :: DOM | e) Element) ->
                             (Element -> Eff (dom :: DOM | e) Element) ->
                             CreateComponent String

foreign import makeRow :: forall e. Array (Array Element) -> Eff (dom :: DOM | e) Element

upperChar :: forall e. Label -> Maybe Char -> UI e (Maybe Char)
upperChar label mc = (head <<< toCharArray) <$> strUi where
  strUi = createUI (cUpperChar nextInput previousInput) label $ maybe "" singleton mc

-- | Creates a Ui for a 1d array, displayed as a table with one row.
rowUi :: forall a e. (a -> UI e a) -> Array a -> UI e (Array a)
rowUi aUi xs = mkUi $ do
  aUis <- traverse (setupFlare <<< aUi) xs
  let signal = traverse _.signal aUis
  let components = map _.components aUis
  table <- makeRow components
  pure $ mkFlare [table] signal



-- MORE OR LESS COPIED FROM purescript-flare BECAUSE IT'S NOT EXPORTED --

foreign import mkUi :: forall e a. Eff (dom :: DOM, channel :: CHANNEL | e) (Flare a) -> UI e a
foreign import mkFlare :: forall a. Array Element -> S.Signal a -> Flare a

type CreateComponent a = forall e. Label
                         -> a
                         -> (a -> Eff (channel :: CHANNEL) Unit)
                         -> Eff (dom :: DOM, channel :: CHANNEL | e) (Array Element)

-- | Set up the HTML element for a given component and create the corresponding
-- | signal channel.
createUI :: forall e a. (CreateComponent a) -> Label -> a -> UI e a
createUI createComps label default = mkUi $ do
  chan <- channel default
  comps <- createComps label default (send chan)
  let signal = subscribe chan
  pure $ mkFlare comps signal


-- GENERAL HTML/DOM STUFF

-- | More convenient `getElementById`
getElement :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElement name = do
  win <- window
  doc <- document win
  getElementById (ElementId name) (htmlDocumentToNonElementParentNode doc)

-- | Remove all children of the given Element
removeAllChildren :: forall e. Node -> Eff (dom :: DOM | e) Unit
removeAllChildren element = do
  childrenNodeList <- childNodes element
  children <- toArray childrenNodeList
  traverse_ (flip removeChild element) children

-- | Render some Smolder markup into a target DOM element.
-- |
-- | This removes all existing children then appends the Smolder markup as new
-- | child nodes.
clobberRender :: forall e.
                 Element ->
                 Markup (EventListener (dom :: DOM | e)) ->
                 Eff (dom :: DOM | e) Unit
clobberRender target markup = do
  removeAllChildren (elementToNode target)
  render target markup


-- | Depth-first iteration over all elements.
-- | Pass `firstChild` and `nextSibling` to iterate forward,
-- | or `lastChild` and `previousSibling` to iterate backward.
-- | Starts over at the beginning (resp. end) when finished.
cycleElement :: forall e.
                (Element -> Eff (dom :: DOM | e) (Maybe Element)) ->  -- `firstChild` or `lastChild`
                (Element -> Eff (dom :: DOM | e) (Maybe Element)) ->  -- `nextSibling` or `previousSibling`
                Element ->  -- current node
                Eff (dom :: DOM | e) Element
cycleElement getChild getSibling = walkDown where
  walkDown n = do
    maybeChild <- getChild n  -- try children first
    case maybeChild of
      Just child -> pure child
      Nothing -> walkUp n -- try sibling, parent next

  walkUp n = do
    maybeSibling <- getSibling n  -- children exhausted, try sibling
    case maybeSibling of
      Just sibling -> pure sibling
      Nothing -> do
        maybeParent <- parentElement (elementToNode n)  -- siblings exhausted, try parent
        case maybeParent of
          Just parent -> walkUp parent
          Nothing -> pure n  -- all done, start again at the top


cycleForward :: forall e. Element -> Eff (dom :: DOM | e) Element
cycleForward =
  cycleElement
    (firstElementChild <<< elementToParentNode)
    (nextElementSibling <<< elementToNonDocumentTypeChildNode)

cycleBackward :: forall e. Element -> Eff (dom :: DOM | e) Element
cycleBackward =
  cycleElement
    (lastElementChild <<< elementToParentNode)
    (previousElementSibling <<< elementToNonDocumentTypeChildNode)

cycleFiltered :: forall e.
                 (Element -> Eff (dom :: DOM | e) Element) ->
                 (Element -> Boolean) ->
                 Element ->
                 Eff (dom :: DOM | e) Element
cycleFiltered cycle pred n =
  -- sanity check to try to avoid infinite loop in case `pred` never matches
  if not (pred n) then pure n
  else
    let nodesListT = LT.unfold (map (\e -> Just (Tuple e e)) <<< cycle) n
    in map (fromMaybe n) $ LT.head $ LT.filter pred nodesListT

isInput :: Element -> Boolean
isInput n = toLower (tagName n) == "input"

nextInput :: forall e. Element -> Eff (dom :: DOM | e) Element
nextInput = cycleFiltered cycleForward isInput

previousInput :: forall e. Element -> Eff (dom :: DOM | e) Element
previousInput = cycleFiltered cycleBackward isInput
