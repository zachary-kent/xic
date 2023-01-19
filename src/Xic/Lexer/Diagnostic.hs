module Xic.Lexer.Diagnostic (diagnostic) where

import Cleff
import Cleff.Error
import Cleff.State
import Conduit
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Conduit.Combinators qualified as Conduit
import Xic.Compile.File qualified as File
import Xic.Compile.Options (Lang)
import Xic.Lexer qualified as Lexer
import Xic.Lexer.Error qualified as Error
import Xic.Lexer.Position (Positioned (..))
import Xic.Lexer.Token qualified as Token

diagnostic :: (MonadUnliftIO m, MonadResource m) => Lang -> FilePath -> ByteString -> ConduitT i o m ()
diagnostic lang outputPath program =
  yieldMany (scan lang program)
    .| Conduit.map show
    .| Conduit.unlines
    .| Conduit.map UTF8.fromString
    .| File.write outputPath

scan :: Lang -> ByteString -> [Positioned String]
scan lang s = fst $ runPure $ runState (Lexer.initialState s) $ scanM lang

scanM :: State Lexer.AlexState :> es => Lang -> Eff es [Positioned String]
scanM lang =
  runError (Lexer.handleLex lang) >>= \case
    Right (Positioned {value = Token.EOF}) -> pure []
    Right token -> (fmap show token :) <$> scanM lang
    Left err -> pure [Error.unError <$> err]
