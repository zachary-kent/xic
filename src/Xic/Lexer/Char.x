{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Xic.Lexer.Char (string) where

import Control.Monad (when)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 qualified as UTF8
import Data.Char (chr, ord)
}

%wrapper "monad"

$hexdigit = [a-fA-F0-9]

tokens :-
  "\\x{" $hexdigit{1,6} \} { withLexeme lexEscapedCodepoint }
  \\ n                     { char '\n' }
  \\ \'                    { char '\'' }
  \\ \\                    { char '\\' }
  \"                       { char '"' }
  \\                       { \_ _ -> invalidEscape }
  .                        { withLexeme $ pure . head }
  
{
withLexeme :: (String -> Alex a) -> AlexAction a
withLexeme tok (_, _, _, lexeme) len = tok $ take len lexeme

char :: token -> AlexAction token
char c = token \_ _ -> c

invalidEscape :: Alex a
invalidEscape = alexError "error:Invalid escape"

readHex :: String -> Int
readHex = read . ("0x" ++)

lexEscapedCodepoint :: String -> Alex Char
lexEscapedCodepoint lexeme = do
  let code = readHex $ init $ drop 2 lexeme
  when (code > ord maxBound) invalidEscape
  pure $ chr code

lexString :: Alex String
lexString = do
  input <- alexGetInput
  sc <- alexGetStartCode
  case alexScan input sc of
    AlexEOF -> pure []
    AlexError _ -> alexError "error:Invalid character constant"
    AlexSkip next _ -> alexSetInput next *> lexString
    AlexToken next len action -> do
      alexSetInput next
      (:) <$> action (ignorePendingBytes input) len <*> lexString

string :: ByteString -> Either String String
string lexeme = runAlex trimmed lexString
  where
    trimmed = init $ tail $ UTF8.toString lexeme

alexEOF :: Alex a
alexEOF = alexError "error:Invalid character constant"
}
