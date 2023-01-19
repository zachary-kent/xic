module Xic.Lexer.Error (Error (..), throwGeneric) where

import Cleff
import Cleff.Error qualified as Cleff
import Xic.Lexer.Position

newtype Error = Error {unError :: String}

generic :: Error
generic = Error "error:Lexical error"

throwGeneric :: Cleff.Error (Positioned Error) :> es => Position -> Eff es a
throwGeneric position =
  Cleff.throwError Positioned {value = generic, position}
