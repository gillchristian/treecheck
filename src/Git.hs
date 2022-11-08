{-# LANGUAGE OverloadedStrings #-}

module Git where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Exit as Sys
import qualified System.Process as Proc

type Failure = (Int, String)

diff :: [FilePath] -> IO (Either Failure String)
diff paths = do
  (code, stdout, stderr) <- Proc.readProcessWithExitCode "git" ("diff" : paths) ""
  pure $ case code of
    Sys.ExitSuccess -> Right stdout
    Sys.ExitFailure i -> Left (i, stderr)

data StagedType
  = StagedModified
  | StagedAdded
  | StagedDeleted

data PendingType
  = PendingModified
  | PendingDeleted

data Status
  = Staged StagedType
  | Pending PendingType
  | Both StagedType PendingType
  | New

type GitStatus = [(FilePath, Status)]

parseChanges :: Text -> GitStatus
parseChanges status = pathAndChangeType <$> Text.lines status
  where
    pending :: Text -> PendingType
    pending "M" = PendingModified
    pending "D" = PendingDeleted
    pending s = error $ "Unrecognized pending status " <> Text.unpack s

    staged :: Text -> StagedType
    staged "M" = StagedModified
    staged "D" = StagedDeleted
    staged "A" = StagedAdded
    staged s = error $ "Unrecognized staged status " <> Text.unpack s

    parse :: Text -> Status
    parse ss = case Text.chunksOf 1 ss of
      [" ", " "] -> error "Unexpected empty status"
      ["?", "?"] -> New
      [" ", p] -> Pending $ pending p
      [s, " "] -> Staged $ staged s
      [s, p] -> Both (staged s) (pending p)
      _ -> error $ "Unexpected status " <> Text.unpack ss

    pathAndChangeType :: Text -> (FilePath, Status)
    pathAndChangeType entry =
      let (status, path) = Text.splitAt 2 entry
       in (Text.unpack path, parse status)

prettyStatus :: GitStatus -> Text
prettyStatus ss = Text.unlines $ line <$> ss
  where
    line :: (FilePath, Status) -> Text
    line (path, New) = "?? " <> Text.pack path
    line (path, Staged s) = staged s <> "  " <> Text.pack path
    line (path, Pending p) = " " <> pending p <> " " <> Text.pack path
    line (path, Both s p) = staged s <> pending p <> " " <> Text.pack path

    pending :: PendingType -> Text
    pending PendingModified = "M"
    pending PendingDeleted = "D"

    staged :: StagedType -> Text
    staged StagedModified = "M"
    staged StagedDeleted = "D"
    staged StagedAdded = "A"

status :: FilePath -> IO (Either Failure GitStatus)
status path = do
  (code, stdout, stderr) <- Proc.readProcessWithExitCode "git" ["status", "-s", path] ""
  pure $ case code of
    Sys.ExitSuccess -> Right $ parseChanges $ Text.pack stdout
    Sys.ExitFailure i -> Left (i, stderr)
