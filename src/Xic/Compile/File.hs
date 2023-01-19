module Xic.Compile.File
  ( ModuleComponent (..),
    File (..),
    isXiSource,
    lexed,
    validateExtension,
    diagnosticPath,
    withContents,
    write,
  )
where

import Conduit
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import System.FilePath
import System.IO.Error
import Xic.Compile.Options

data ModuleComponent = Source | Interface

data File = File
  { lang :: Lang,
    moduleComponent :: ModuleComponent,
    path :: FilePath
  }

isXiSource :: File -> Bool
isXiSource (File Xi Source _) = True
isXiSource _ = False

validateExtension :: FilePath -> Maybe File
validateExtension path =
  case takeExtension path of
    "xi" -> Just $ File Xi Source path
    "ixi" -> Just $ File Xi Interface path
    "rh" -> Just $ File Rho Source path
    "ri" -> Just $ File Rho Interface path
    _ -> Nothing

diagnosticPath :: Options -> FilePath -> FilePath
diagnosticPath Options {diagnosticDirectory} path =
  case diagnosticDirectory of
    Just dir -> dir </> path
    Nothing -> path

lexed :: Options -> FilePath -> FilePath
lexed options path = diagnosticPath options $ path -<.> "lexed"

errorMessage :: FilePath -> IOError -> String
errorMessage path e = "Error: " ++ path ++ " " ++ annotate e
  where
    annotate e
      | isAlreadyExistsError e = "already exists"
      | isDoesNotExistError e = "does not exist"
      | isAlreadyInUseError e = "is already in use"
      | isPermissionError e = "requires greater permissions"
      | otherwise = "cannot be accessed"

handled :: MonadUnliftIO m => FilePath -> ConduitT i o m () -> ConduitT i o m ()
handled path = handleC $ liftIO . putStrLn . errorMessage path

withContents :: (MonadResource m, MonadUnliftIO m) => FilePath -> (Lazy.ByteString -> ConduitT i o m ()) -> ConduitT i o m ()
withContents path f = handled path $ f =<< sourceFile path .| sinkLazy

write :: (MonadUnliftIO m, MonadResource m) => FilePath -> ConduitT ByteString o m ()
write path = handled path $ sinkFile path
