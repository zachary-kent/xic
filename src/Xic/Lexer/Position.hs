module Xic.Lexer.Position (Position (..), Positioned (..)) where

import Formatting

data Position = Position
  { line :: Int,
    column :: Int
  }

instance Show Position where
  show Position {line, column} = formatToString (int % ":" % int) line column

data Positioned a = Positioned
  { value :: a,
    position :: Position
  }
  deriving (Functor)

instance Show a => Show (Positioned a) where
  show Positioned {value, position} =
    formatToString (shown % " " % shown) position value
