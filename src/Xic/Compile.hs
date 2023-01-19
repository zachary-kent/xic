module Xic.Compile (compile) where

import Control.Monad
import Data.Conduit
import Xic.Compile.File (File (File))
import Xic.Compile.File qualified as File
import Xic.Compile.Options
import Xic.Compile.Options qualified as Options
import Xic.Lexer.Diagnostic qualified as Lexer

invalidFileExtension :: FilePath -> String
invalidFileExtension path = "Error: invalid file extension: " ++ path

compileFile :: Options -> File -> IO ()
compileFile options@Options {lex} file@File {lang, path} =
  runConduitRes $ File.withContents path \contents ->
    when (lex && File.isXiSource file) do
      let lexedPath = File.lexed options path
      Lexer.diagnostic lang lexedPath contents

withOptions :: Options -> IO ()
withOptions options@Options {files} =
  forM_ files \path ->
    case File.validateExtension path of
      Nothing -> putStrLn $ invalidFileExtension path
      Just file -> compileFile options file

compile :: IO ()
compile = withOptions =<< Options.parse
