module Xic.Lexer.Diagnostic (scan) where

import Cleff
import Cleff.Error
import Cleff.State
import Data.ByteString.Lazy (ByteString)
import Data.Conduit
import Xic.Compile.Options (Lang)
import Xic.Lexer qualified as Lexer
import Xic.Lexer.Error qualified as Error
import Xic.Lexer.Position (Positioned (..))
import Xic.Lexer.Token qualified as Token

diagnostic :: Lang -> ByteString -> [String]
diagnostic lang = map show . scan lang

scan :: Lang -> ByteString -> [Positioned String]
scan lang s = fst $ runPure $ runState (Lexer.initialState s) $ scanM lang

scanM :: State Lexer.AlexState :> es => Lang -> Eff es [Positioned String]
scanM lang =
  runError (Lexer.handleLex lang) >>= \case
    Right (Positioned {value = Token.EOF}) -> pure []
    Right token -> (fmap show token :) <$> scanM lang
    Left err -> pure [Error.unError <$> err]
