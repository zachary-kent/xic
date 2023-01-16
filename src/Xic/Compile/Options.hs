module Xic.Compile.Options (Lang (..)) where

-- | The langauge to compile: either Xi or its extension Rho
data Lang = Xi | Rho
  deriving (Eq)
