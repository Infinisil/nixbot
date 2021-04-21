{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Commands.Find
  ( findFunction
  , Find
  , findHandle
  , findParser
  ) where

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Char                      (isSpace)
import           Data.Functor                   (void, ($>))
import           Data.List
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Void
import           GitHub
import           GitHub.Data.Name
import qualified GitHub.Endpoints.Repos.Commits as C
import           Plugins
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Types

type Parser = Parsec Void String

parseWord :: Parser String
parseWord = some (satisfy (not . isSpace)) <* (void space1 <|> eof)

data Find = FindHelp
          | FindPath String
          deriving (Show)

findParser :: Parser Find
findParser = eof $> FindHelp
  <|> FindPath <$> parseWord

getNixpkgs :: MonadIO m => Text -> m (Maybe Text)
getNixpkgs s = do
  c <- liftIO $ executeRequest' (C.commitR "NixOS" "nixpkgs" "HEAD")
  case c of
    Left err -> do
      liftIO $ print err
      return Nothing
    Right Commit { commitSha = N sha } ->
      return $ Just $ "https://github.com/NixOS/nixpkgs/tree/" <> Text.take 8 sha <> s

findPath :: [Text] -> Text -> Maybe Text
findPath cache input = find (Text.isSuffixOf input) cache

prependSlash :: Text -> Text
prependSlash = ("/"<>) . snd . Text.break (/='/')

nixpkgs :: MonadIO m => [Text] -> [String] -> m [Text]
nixpkgs cache s = do
  let searches = mapMaybe (findPath cache . prependSlash . Text.pack) s
  catMaybes <$> mapM getNixpkgs searches

findFunction :: MonadIO m => [Text] -> [String] -> m [String]
findFunction _ [] = return []
findFunction cache args = do
  ree <- nixpkgs cache args
  case ree of
    []      -> return ["Couldn't find any such files"]
    results -> return $ map (("Found file: " ++) . Text.unpack) results


findHandle :: Find -> PluginT App ()
findHandle _ = reply ",find is temporarily unimplemented"
{-findHandle FindHelp = reply "Use ,find to get a GitHub link to the master version of files in nixpkgs, e.g. \",find hello/default.nix\" or \",find all-packages.nix\""
findHandle (FindPath path) = do
  return ()-}
