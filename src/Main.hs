{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}


module Main where

import           Config
import           IRC
import           Log
import           Plugins
import           Plugins.Commands
import           Plugins.Commands.Tell
import           Plugins.Karma
import           Plugins.Leaked
import           Plugins.NixRepl
import           Plugins.Pr
import           Plugins.Unreg
import           Types

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Set                       as Set
import           Frontend.AMQP
import           Frontend.Types
import           System.Directory
import           System.FilePath

withSharedState :: Config -> (TVar SharedState -> IO a) -> IO a
withSharedState cfg action = do
  let path = configStateDir cfg </> "new/shared"
  exists <- doesFileExist path
  value <- if exists then eitherDecodeFileStrict path >>= \case
    Left err -> fail $ "Error decoding shared state file (" ++ path ++ "): " ++ err
    Right value -> return value
  else return $ SharedState Set.empty
  var <- newTVarIO value

  result <- action var

  finalValue <- readTVarIO var
  encodeFile path finalValue
  putStrLn "Saved shared state"
  return result



main :: IO ()
main = do
  config' <- getConfig
  logQueue' <- newTMQueueIO
  withSharedState config' $ \sharedState' -> do
    frontend' <- initFrontend
    let env = Env config' logQueue' sharedState' frontend'
    flip runReaderT env $ withLogging $
      runFrontend onInput

onInput :: Input -> App ()
onInput input = do
  traceUser input
  ps <- plugins (inputSender input)
  _ <- runPlugins ps input
  return ()

traceUser :: Input -> App ()
traceUser (inputUser -> user) = do
  var <- asks sharedState
  new <- liftIO $ atomically $ do
    s <- readTVar var
    writeTVar var $ s { knownUsers = Set.insert user (knownUsers s) }
    return $ not $ Set.member user $ knownUsers s
  when new $ logMsg $ "Recorded new user: " <> user

examplePlugin :: Plugin
examplePlugin = Plugin
  { pluginName = "example"
  , pluginCatcher = Catched True
  , pluginHandler = \Input { inputSender } ->
      case inputSender of
        Left "infinisil" -> privMsg "infinisil" "I have received your message"
        Right ("bottest", user) -> chanMsg "bottest" $ user <> ": I have received your message"
        _ -> return ()
  }

developFilter :: Plugin
developFilter = Plugin
  { pluginName = "develop-filter"
  , pluginCatcher = \Input { inputSender } -> case inputSender of
      Left "infinisil"     -> PassedOn
      Left "gchristensen"  -> PassedOn
      Right ("bottest", _) -> PassedOn
      _                    -> Catched True ()
  , pluginHandler = const (return ())
  }

plugins :: (MonadIO m, MonadReader Env m) => Either User (Channel, User) -> m [Plugin]
plugins sender = do
  pluginConfig <- asks (pluginConfigForSender sender . config)

  debug <- asks (configDebugMode . config)

  let selectedPlugins =
        [ developFilter | debug ] ++
        [ unregPlugin | enableUnreg pluginConfig ] ++
        [ tellSnooper | enableCommands pluginConfig ] ++
        [ leakedPlugin | enableLeaked pluginConfig ] ++
        [ commandsPlugin' | enableCommands pluginConfig ] ++
        [ nixreplPlugin | enableNixrepl pluginConfig ] ++
        [ karmaPlugin | enableKarma pluginConfig ] ++
        [ prPlugin (configPr pluginConfig) | enablePr pluginConfig ]

  --liftIO $ putStrLn $ "For sender " ++ show sender ++ " using plugins " ++ show (map pluginName selectedPlugins)
  return selectedPlugins


