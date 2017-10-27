module Acrostic.Gist (
  GistError(..),
  GistId(..),
  apiBasePath,
  loadPuzzleFromGist,
  loadPuzzleFromQueryString,
  postPuzzleToGist
) where

import Prelude

import Acrostic.Puzzle (Puzzle, defaultPuzzle, fromJson, toJson)
import Control.Monad.Aff (Aff, Error, catchError, try)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (ExceptT(..), except, lift, runExcept, withExceptT)
import DOM (DOM)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get, post)
import Simple.JSON (readJSON, writeJSON)
import Try.QueryString (getQueryStringMaybe)

newtype GistId = GistId String

type GistPayload = {
  files :: {
    puzzle :: {
      content:: String
    }
  }
} 

type GistResponse = {
  id :: String
}

-- | Two types of things could go wrong:
data GistError =
  AjaxErr Error
  | JsonErr MultipleErrors

instance gistString :: Show GistError where 
  show (AjaxErr e) = "Ajax Error: " <> show e
  show (JsonErr e) = "Json Error:" <> show e 

toUrl :: GistId -> String
toUrl (GistId i) = apiBasePath <> "/" <> i

apiBasePath :: String
apiBasePath = "https://api.github.com/gists"

-- Loading methods
gistPayloadFromJson :: forall e. String ->
                       ExceptT GistError (Aff (ajax :: AJAX | e)) GistPayload
gistPayloadFromJson json = (runExcept $ readJSON json) # except # withExceptT JsonErr

loadPuzzleFromGist :: forall e. GistId ->
                      ExceptT GistError (Aff (ajax :: AJAX | e)) Puzzle
loadPuzzleFromGist gid = do
  let url = toUrl gid
  r <- (try $ get url) # ExceptT # withExceptT AjaxErr
  payload <- gistPayloadFromJson r.response
  (fromJson payload.files.puzzle.content) # except # withExceptT JsonErr

-- | looks up the "gist" querystring in the current url location,
-- | then fetches that gist from github api.
-- | If "gist" querystring is missing or ajax call fails,
-- | logs error and returns `defaultPuzzle`.
loadPuzzleFromQueryString ::
  forall e. ExceptT GistError (Aff (ajax :: AJAX,
                                    console :: CONSOLE,
                                    dom :: DOM | e)) Puzzle
loadPuzzleFromQueryString = do
  let default = pure defaultPuzzle
  gistId <- liftEff $ getQueryStringMaybe "gist"
  case gistId of
    Just id -> loadPuzzleFromGist (GistId id) `catchError` \e -> do
      lift $ logShow e
      default
    Nothing -> default

-- Saving methods
puzzleToGist :: Puzzle -> GistPayload
puzzleToGist puzzle = { files: { puzzle: { content: toJson puzzle } } }

gistResponseFromJson :: forall e. String ->
                        ExceptT GistError (Aff (ajax :: AJAX | e)) GistResponse
gistResponseFromJson json = (runExcept $ readJSON json) # except # withExceptT JsonErr

postPuzzleToGist :: forall e. Puzzle ->
                    ExceptT GistError (Aff (ajax :: AJAX | e)) String
postPuzzleToGist puzzle = do
  let json = writeJSON $ puzzleToGist puzzle
  r <- (try $ post (apiBasePath) json) # ExceptT # withExceptT AjaxErr
  gistResponse <- gistResponseFromJson r.response
  pure $ gistResponse.id

-- | only keep the first error if there are multiple errors
instance semigroupGistError :: Semigroup GistError where
  append = const
