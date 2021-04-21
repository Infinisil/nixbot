{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.Pr (prPlugin, Settings(..), ParsedIssue(..), ParseType(..)) where

import           Frontend.Types
import           Plugins

import           Config
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time
import           GitHub               hiding (Owner, Repo)
import           GitHub.Data.Name
import           Log
import           Text.Regex.TDFA
import           Types
import           Utils

fetchInfo :: Settings -> ParsedIssue -> PluginT App (Maybe Text)
fetchInfo Settings { defOwner, defRepo } (ParsedIssue parseType owner repo number) = do
  lift $ logMsg $ "Fetching info for issue " <> owner <> "/" <> repo <> "#" <> Text.pack (show number)
  result <- liftIO . executeRequest' $ issueR owner' repo' number'
  case result of
    Left err -> do
      lift $ logMsg $ "Got error from github request (might indicate that this issue doesn't exist): " <> Text.pack (show err)
      return Nothing
    Right issue@Issue { issueHtmlUrl = Nothing } -> do
      lift $ logMsg $ "Issue didn't have an url for some reason: " <> Text.pack (show issue)
      return Nothing
    Right issue@Issue { issueHtmlUrl = Just url } -> Just <$> do
      state <- case (issuePullRequest issue, issueState issue) of
        (Just _, StateClosed) -> do
          lift $ logMsg "This issue is a PR and closed, fetching to see if it's merged"
          pullResult <- liftIO $ executeRequest' $ pullRequestR owner' repo' (issueNumber issue)
          return $ case pullResult of
            Right PullRequest { pullRequestMerged = True } -> "merged"
            _                                              -> showState
        _ -> return showState
      ago <- liftIO $ fmap (prettySeconds 1 . round . (`diffUTCTime` issueCreatedAt issue)) getCurrentTime
      return $ prefix <> " (by " <> author <> ", " <> ago <> " ago, " <> state <> "): " <> title
      where
        author = untagName . simpleUserLogin . issueUser $ issue
        title = issueTitle issue
        prefix = case parseType of
          Hash -> getUrl url
          Link -> case (defOwner repo == owner, defRepo == repo) of
            (False, _)    -> owner <> "/" <> repo <> "#" <> Text.pack (show number)
            (True, False) -> repo <> "#" <> Text.pack (show number)
            (True, True)  -> "#" <> Text.pack (show number)
        showState = case issueState issue of
          StateClosed -> "closed"
          StateOpen   -> "open"
  where
    owner' = N owner
    repo' = N repo
    number' = IssueNumber number

type Owner = Text
type Repo = Text
data ParseType = Hash | Link deriving Show

data ParsedIssue = ParsedIssue ParseType Owner Repo Int deriving Show

sameIssue :: ParsedIssue -> ParsedIssue -> Bool
sameIssue (ParsedIssue _ owner1 repo1 num1) (ParsedIssue _ owner2 repo2 num2) =
  owner1 == owner2 && repo1 == repo2 && num1 == num2

parseIssues :: Settings -> String -> [ParsedIssue]
parseIssues Settings { defOwner, defRepo } = map (extractIssue . map Text.pack)  . match prRegex
  where
    prRegex :: RegexMaker a CompOption ExecOption String => a
    prRegex = makeRegex $ "(([^ ()]+)/)?([^ ()]+)?#([[:digit:]]+)\\>"
      ++ "|" ++ "https://github.com/([^/ ]+)/([^/ ]+)/(issues|pull)/([[:digit:]]+)([^#/[:digit:]]|\\')"

    extractIssue :: [Text] -> ParsedIssue
    extractIssue [_, _, _, _, "", owner, repo, _, numberStr, _] =
      ParsedIssue Link owner repo (read (Text.unpack numberStr))
    extractIssue [_, _, owner, repo, numberStr, _, _, _, _, _]
      | Text.null repo = ParsedIssue Hash (defOwner defRepo) defRepo number
      | Text.null owner = ParsedIssue Hash (defOwner repo) repo number
      | otherwise = ParsedIssue Hash owner repo number
      where number = read (Text.unpack numberStr)
    extractIssue m = error $ "The pull request regex has a bug, the following part was matched but not caught by any patterns: " ++ Text.unpack (head m)

data Settings = Settings
  { defOwner :: Repo -> Owner
  , defRepo  :: Repo
  , prFilter :: ParsedIssue -> Bool
  }

configToSettings :: PrConfig -> Settings
configToSettings PrConfig
  { configIgnoreStandaloneUnder
  , configDefaultRepo
  , configDefaultOwners
  , configFallbackOwner }
  = Settings
  { defOwner = lookupRepo
  , defRepo = configDefaultRepo
  , prFilter = \case
      ParsedIssue Hash owner repo number ->
        repo /= configDefaultRepo ||
        owner /= configFallbackOwner ||
        number >= configIgnoreStandaloneUnder
      _ -> True
  }
  where
    lookupRepo repo = Map.findWithDefault configFallbackOwner repo configDefaultOwners

prPlugin :: PrConfig -> Plugin
prPlugin config = Plugin
  { pluginName = "pr"
  , pluginCatcher = \Input { inputMessage } ->
      case filter (prFilter settings) . nubBy sameIssue $ parseIssues settings (Text.unpack inputMessage) of
        []     -> PassedOn
        result -> Catched True result
  , pluginHandler = \result ->
      forM_ result $ fetchInfo settings >=> \case
        Nothing -> return ()
        Just msg -> reply msg
  }
  where settings = configToSettings config
