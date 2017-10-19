module Acrostic.Gist (
  GistError(..),
  GistId(..),
  apiBasePath,
  defaultGistId,
  loadPuzzleFromGist,
  savePuzzleToGist
) where

import Prelude

import Acrostic.Puzzle (Puzzle, fromJson, toJson, defaultPuzzle)
import Control.Monad.Aff (Aff, Error, try)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (ExceptT(..), except, withExceptT)
import DOM.Node.Document (url)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.HTTP.Method (Method(..))
import Data.JSDate (jsdate)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, Affjax, AffjaxResponse, affjax, get, patch, post, defaultRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode(..))
import Simple.JSON (class WriteForeign, writeJSON)
import Text.Smolder.SVG (a)
import Text.Smolder.SVG.Attributes (offset)

newtype GistId = GistId String

-- | Two types of things could go wrong:
data GistError =
  AjaxErr Error
  | JsonErr MultipleErrors

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
                     
jsonWrapper :: Puzzle -> String
jsonWrapper puzzle = do
  writeJSON $ {
    "files": {
      "puzzle.json":{
        "content": puzzle
      }
    }
  }

savePuzzleToGist :: forall e.                                               
  GistId -> Puzzle -> Aff ( ajax :: AJAX | e) (AffjaxResponse Unit)
savePuzzleToGist gid puzzle = do
  affjax $ defaultRequest { 
    method = Left PATCH, 
    url = toUrl gid, 
    content = Just $ jsonWrapper puzzle, 
    headers = [RequestHeader "Authorization" "token d3045b5f8d6c2c73a90e12f5768f1a4e4d12e46d"] 
  }

-- I made this gist by hand, so it's probably not in the right format
defaultGistId :: GistId
defaultGistId = GistId "934c85b24e41d12767391a1aa7e3ab62"

-- | only keep the first error if there are multiple errors
instance semigroupGistError :: Semigroup GistError where
  append = const
