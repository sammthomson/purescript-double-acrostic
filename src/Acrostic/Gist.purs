module Acrostic.Gist (
  GistError(..),
  GistId(..),
  apiBasePath,
  defaultGistId,
  loadPuzzleFromGist
) where

import Prelude

import Acrostic.Puzzle (Puzzle, fromJson)
import Control.Monad.Aff (Aff, Error, try)
import Control.Monad.Except (ExceptT(..), except, withExceptT)
import Data.Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Network.HTTP.Affjax (AJAX, get)

newtype GistId = GistId String

-- | Two types of things could go wrong:
data GistError =
  AjaxErr Error
  | JsonErr (NonEmptyList ForeignError)

toUrl :: GistId -> String
toUrl (GistId i) = apiBasePath <> i

apiBasePath :: String
apiBasePath = "https://api.github.com/gists/"

loadPuzzleFromGist :: forall e. GistId ->
                      ExceptT GistError (Aff (ajax :: AJAX | e)) Puzzle
loadPuzzleFromGist gid = do
  let url = toUrl gid
  r <- (try $ get url) # ExceptT # withExceptT AjaxErr
  fromJson r.response # except # withExceptT JsonErr

defaultGistId :: GistId
defaultGistId = GistId "0b9abb47ec8815a089fd917afdc47e9f"

-- | only keep the first error if there are multiple errors
instance semigroupGistError :: Semigroup GistError where
  append = const
