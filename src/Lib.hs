{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import qualified Data.List as List
import Text.Casing (camel)
import qualified Data.Bifunctor as BF
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import qualified Data.Aeson as Json
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Process as Proc
import qualified System.Exit as Sys
import qualified System.FilePath as Path
import System.FilePath ((</>))
import Control.Monad (when, unless, void)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

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

data FileChangeType
  = Modified
  | Deleted
  | Added
  | Staged

data ProjectCheckFailure
  = FilesChanged [(FilePath, FileChangeType)]
  | CmdError String Int
  | OtherError String Int
  | HadPreviousChanges String

gitDiff :: [FilePath] -> IO (Sys.ExitCode, String, String)
gitDiff paths = Proc.readProcessWithExitCode "git" ("diff" : paths) ""

gitStatus :: FilePath -> IO (Sys.ExitCode, String, String)
gitStatus path = Proc.readProcessWithExitCode "git" ["status", "-s", path] ""

parseChanges :: Text -> [(FilePath, FileChangeType)]
parseChanges status = pathAndChangeType . Text.stripStart <$> Text.lines status
  where
  pathAndChangeType :: Text -> (FilePath, FileChangeType)
  pathAndChangeType entry =
    let [status, path] = Text.words entry
     in (Text.unpack path, parse status)

  parse :: Text -> FileChangeType
  parse s
    | "A" `Text.isPrefixOf` s = Staged
    | "M" `Text.isPrefixOf` s = Modified
    | "D" `Text.isPrefixOf` s = Deleted
    | "?" `Text.isPrefixOf` s = Added
    | otherwise = error $ "Unrecognized status " <> Text.unpack s

checkProject :: Text -> Project -> IO (Either ProjectCheckFailure ())
checkProject name Project { projectDirectory, projectCommand, projectOutput } =
  Dir.withCurrentDirectory (Text.unpack $ fromMaybe  "." projectDirectory) $ do
    Text.putStrLn $ "[" <> name <> "] Running"

    -- TODO: validate this instead of unsafe pattern match
    let (program:args) = words $ Text.unpack projectCommand
    (cmdCode, _cmdStdOut, cmdStdErr) <- Proc.readProcessWithExitCode program args ""

    (statusCode, status, _) <- gitStatus $ Text.unpack projectOutput

    let changes = parseChanges $ Text.strip $ Text.pack status
        hasChanges = not $ null changes

    when hasChanges $ do
      Text.putStrLn $ "[" <> name <> "] Failed"
      putStrLn ""
      putStrLn status

    unless hasChanges $ do
      Text.putStrLn $ "[" <> name <> "] Success"

    case (hasChanges, cmdCode, statusCode) of
      (False, _, _) -> pure $ Right ()
      (_, Sys.ExitFailure n, _) -> pure $ Left $ CmdError cmdStdErr n
      (_, _, Sys.ExitFailure n) -> pure $ Left $ OtherError cmdStdErr n
      (True, _, _) -> pure $ Left $ FilesChanged changes

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
    Right projects -> void $ Map.traverseWithKey checkProject projects
    Left err -> Sys.die err

runOn :: FilePath -> IO ()
runOn dir = do
  Dir.setCurrentDirectory dir
  putStrLn . ("Running on " <>) =<< Dir.getCurrentDirectory
  putStrLn ""
  run
