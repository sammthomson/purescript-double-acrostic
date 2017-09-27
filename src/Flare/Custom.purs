module Flare.Custom (
  Ui,
  upperChar,
  rowUi
) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Element)
import Data.Array (head)
import Data.Maybe (Maybe, maybe)
import Data.String (singleton, toCharArray)
import Data.Traversable (traverse)
import Flare (Flare, Label, UI, setupFlare)
import Signal as S
import Signal.Channel (CHANNEL, subscribe, send, channel)

-- | A UI with any effects during setup (or none)
type Ui a = forall e. UI e a

foreign import cUpperChar :: CreateComponent String

foreign import makeRow :: forall e. Array (Array Element) -> Eff (dom :: DOM | e) Element

upperChar :: Label -> Maybe Char -> Ui (Maybe Char)
upperChar label mc = (head <<< toCharArray) <$> strUi where
  strUi = createUI cUpperChar label $ maybe "" singleton mc

-- | Creates a Ui for a 1d array, displayed as a table with one row.
rowUi :: forall a. (a -> Ui a) -> Array a -> Ui (Array a)
rowUi aUi xs = mkUi $ do
  aUis <- traverse (setupFlare <<< aUi) xs
  let signal = traverse _.signal aUis
  let components = map _.components aUis
  table <- makeRow components
  pure $ mkFlare [table] signal



-- MORE OR LESS COPIED FROM purescript-flare BECAUSE IT'S NOT EXPORTED --

foreign import mkUi :: forall e a. Eff (dom :: DOM, channel :: CHANNEL | e) (Flare a) -> Ui a
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
