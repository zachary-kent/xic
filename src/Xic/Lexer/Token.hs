module Xic.Lexer.Token (Token (..)) where

import Data.Char
import Data.Int (Int64)
import Formatting
import Prelude hiding (Ordering (..))

-- | A token of the Xi language
data Token
  = USE
  | IF
  | ELSE
  | WHILE
  | RETURN
  | BREAK
  | RECORD
  | LENGTH
  | NULL
  | CHAR Char
  | STRING String
  | INT Int64
  | BOOL Bool
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | GETS
  | DOT
  | MULT
  | HIGHMULT
  | DIV
  | MOD
  | PLUS
  | MINUS
  | LT
  | LEQ
  | GEQ
  | GT
  | EQ
  | NEQ
  | NOT
  | AND
  | OR
  | COLON
  | SEMICOLON
  | COMMA
  | ID String
  | WILDCARD
  | TYINT
  | TYBOOL
  | EOF

escapeChar :: Char -> String
escapeChar '\n' = "\\n"
escapeChar '\'' = "\\'"
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar c
  | isPrint c = [c]
  | otherwise = formatToString ("\\x{" % hex % "}") $ ord c

formatChar :: Char -> String
formatChar c = "character " ++ escapeChar c

escapeString :: String -> String
escapeString = concatMap escapeChar

formatString :: String -> String
formatString s = "string " ++ escapeString s

formatInt :: Int64 -> String
formatInt = formatToString ("integer " % int)

formatIdentifier :: String -> String
formatIdentifier = formatToString ("id " % string)

instance Show Token where
  show = \case
    USE -> "use"
    IF -> "if"
    ELSE -> "else"
    WHILE -> "while"
    RETURN -> "return"
    BREAK -> "break"
    RECORD -> "record"
    LENGTH -> "length"
    NULL -> "null"
    CHAR c -> formatChar c
    STRING s -> formatString s
    INT i -> formatInt i
    BOOL True -> "true"
    BOOL False -> "false"
    LPAREN -> "("
    RPAREN -> ")"
    LBRACK -> "["
    RBRACK -> "]"
    LBRACE -> "{"
    RBRACE -> "}"
    GETS -> "="
    DOT -> "."
    MULT -> "*"
    HIGHMULT -> "*>>"
    DIV -> "/"
    MOD -> "%"
    PLUS -> "+"
    MINUS -> "-"
    LT -> "<"
    LEQ -> "<="
    GEQ -> ">="
    GT -> ">"
    EQ -> "=="
    NEQ -> "!="
    NOT -> "!"
    AND -> "&"
    OR -> "|"
    COLON -> ":"
    SEMICOLON -> ";"
    COMMA -> ","
    ID ident -> formatIdentifier ident
    WILDCARD -> "_"
    TYINT -> "int"
    TYBOOL -> "bool"
    EOF -> "eof"
