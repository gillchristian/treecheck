{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Aeson as Json
import qualified Data.Bifunctor as BF
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Git
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Sys
import System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified System.IO as Sys
import qualified System.Process as Proc
import Text.Casing (camel)

dropLabelPrefix :: String -> Json.Options
dropLabelPrefix prefix =
  Json.defaultOptions {Json.fieldLabelModifier = camel . stripPrefix prefix}

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix str = Maybe.fromMaybe str $ List.stripPrefix prefix str

data Project = Project
  { projectCommand :: Text,
    projectDirectory :: Maybe Text,
    projectOutput :: Text
  }
  deriving (Eq, Show, Generic)

instance Json.ToJSON Project where
  toJSON = Json.genericToJSON $ dropLabelPrefix "project"

instance Json.FromJSON Project where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "project"

type Config = Map Text Project

readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile path =
  BF.first Yaml.prettyPrintParseException
    <$> Yaml.decodeFileEither path

data ProjectCheckFailure
  = HadPreviousChanges Git.GitStatus
  | FilesChanged Git.GitStatus
  | CmdError (Int, String)
  | OtherError (Int, String)

checkStatus :: Text -> Project -> ExceptT ProjectCheckFailure IO ()
checkStatus _ Project {..} = do
  prevChanges <- liftIO $ Git.status $ Text.unpack projectOutput

  case prevChanges of
    Right changes | null changes -> pure ()
    Right changes -> throwE $ HadPreviousChanges changes
    Left err -> throwE $ OtherError err

runGen :: Text -> Project -> ExceptT ProjectCheckFailure IO ()
runGen name Project {..} = do
  let (program : args) = words $ Text.unpack projectCommand
  (cmdCode, _cmdStdOut, cmdStdErr) <- liftIO $ Proc.readProcessWithExitCode program args ""

  case cmdCode of
    Sys.ExitFailure code -> throwE $ CmdError (code, cmdStdErr)
    _ -> pure ()

checkDiff :: Text -> Project -> ExceptT ProjectCheckFailure IO ()
checkDiff name Project {..} = do
  status <- liftIO $ Git.status $ Text.unpack projectOutput

  case status of
    Left (code, stderr) ->
      throwE $ OtherError (code, stderr)
    Right changes
      | not $ null changes ->
          throwE $ FilesChanged changes
    _ -> pure ()

reportProjectResult :: Text -> Project -> ProjectCheckFailure -> IO ()
reportProjectResult name Project {..} result = do
  Text.putStrLn $ "[" <> name <> "] FAIL"
  case result of
    HadPreviousChanges changes -> do
      Text.putStrLn $ "[" <> name <> "] Directory is not clean"
      Text.putStrLn $ "[" <> name <> "] Make sure to run treecheck with no pending changes"
      putStrLn ""
      Text.putStrLn $ Git.prettyStatus changes
    FilesChanged changes -> do
      Text.putStrLn $ "[" <> name <> "] Generated code not up to date"
      Text.putStrLn $ "[" <> name <> "] Run " <> projectCommand <> " to fix"
      putStrLn ""
      Text.putStrLn $ Git.prettyStatus changes
    CmdError (code, stderr) -> do
      Text.putStrLn $ "[" <> name <> "] Generate command failed"
      putStrLn ""
      Sys.hPutStrLn Sys.stderr stderr
    OtherError (code, stderr) -> do
      Text.putStrLn $ "[" <> name <> "] Unexpected error"
      putStrLn ""
      Sys.hPutStrLn Sys.stderr stderr

runProject :: Text -> Project -> IO Bool
runProject name project@Project {..} =
  Dir.withCurrentDirectory (Text.unpack $ fromMaybe "." projectDirectory) $ do
    result <- runExceptT $ do
      checkStatus name project
      liftIO $ Text.putStrLn $ "[" <> name <> "] Running"
      runGen name project
      checkDiff name project

    case result of
      Left err -> do
        reportProjectResult name project err
        pure False
      Right _ -> do
        Text.putStrLn $ "[" <> name <> "] SUCCESS"
        pure True

-- | Recursively look for a file the current directory and all the parents, up
-- to the current user's home directory.
findParentDirContaining :: FilePath -> IO (Maybe FilePath)
findParentDirContaining file = do
  initial <- Dir.getCurrentDirectory
  Dir.withCurrentDirectory initial go
  where
    go = do
      current <- Dir.getCurrentDirectory
      found <- Dir.doesFileExist $ current </> file

      -- Should drop both trailing path separator on both `home` and `parent`
      -- to avoid failing the check and ending up on an infinite loop
      home <- Path.dropTrailingPathSeparator <$> Dir.getHomeDirectory
      let parent = Path.dropTrailingPathSeparator $ Path.dropFileName current

      case (found, home == parent) of
        (True, _) -> pure $ Just current
        (_, True) -> pure Nothing
        (_, False) -> do
          Dir.setCurrentDirectory parent
          go

treecheckFile :: FilePath
treecheckFile = "treecheck.yaml"

run :: IO ()
run = do
  mbDir <- findParentDirContaining treecheckFile
  case mbDir of
    Just dir -> Dir.setCurrentDirectory dir
    Nothing -> Sys.die $ "Could not find " <> treecheckFile

  eProjects <- readConfigFile treecheckFile
  case eProjects of
    Right projects -> void $ Map.traverseWithKey runProject projects
    Left err -> Sys.die err

runOn :: FilePath -> IO ()
runOn dir = do
  Dir.setCurrentDirectory dir
  putStrLn . ("Running on " <>) =<< Dir.getCurrentDirectory
  putStrLn ""
  run
