module Xic.Compile.Options (Lang (..), Options (..), parse) where

import Options.Applicative

-- | The langauge to compile: either Xi or its extension Rho
data Lang = Xi | Rho
  deriving (Eq)

data Options = Options
  { lex :: Bool,
    diagnosticDirectory :: Maybe FilePath,
    files :: [FilePath]
  }

parser :: Parser Options
parser = do
  let lexHelp = "Generate output from lexical analysis"
      dirHelp = "Specify where to place generated diagnostic files"
  lex <- switch $ long "lex" <> help lexHelp
  diagnosticDirectory <- optional $ strOption $ short 'D' <> help dirHelp <> metavar "<path>"
  files <- some $ argument str $ metavar "<source files>"
  pure Options {lex, diagnosticDirectory, files}

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper) modifiers
  where
    modifiers = fullDesc <> progDesc "Compile Xi language files"

parse :: IO Options
parse = execParser parserInfo
