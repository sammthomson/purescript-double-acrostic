module Flare.Custom (
  getElement,
  upperChar,
  rowUi
) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(ElementId))
import Data.Array (head)
import Data.Maybe (Maybe, maybe)
import Data.String (singleton, toCharArray)
import Data.Traversable (traverse)
import Flare (Flare, Label, UI, setupFlare)
import Signal as S
import Signal.Channel (CHANNEL, subscribe, send, channel)


foreign import cUpperChar :: CreateComponent String

foreign import makeRow :: forall e. Array (Array Element) -> Eff (dom :: DOM | e) Element

upperChar :: forall e. Label -> Maybe Char -> UI e (Maybe Char)
upperChar label mc = (head <<< toCharArray) <$> strUi where
  strUi = createUI cUpperChar label $ maybe "" singleton mc

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


-- | More convenient `getElementById`
getElement :: forall e. String -> Eff (dom :: DOM | e) (Maybe Element)
getElement name = do
  win <- window
  doc <- document win
  getElementById (ElementId name) (htmlDocumentToNonElementParentNode doc)
