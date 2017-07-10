module Data.Dictionary where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Newtype (class Newtype)
import Data.Set (Set, fromFoldable)
import Data.String (Pattern(..), split, trim)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)


newtype Dictionary = Dictionary (Set String)

derive instance newtypeDictionary :: Newtype Dictionary _

defaultPath :: FilePath
defaultPath = "/usr/share/dict/words"


loadDict :: forall e. FilePath -> Eff (fs :: FS, exception :: EXCEPTION | e) Dictionary
loadDict path = do
  text <- readTextFile UTF8 path
  pure $ Dictionary (fromFoldable (split (Pattern "\n") (trim text)))
