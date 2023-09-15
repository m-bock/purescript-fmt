module Fmt.Config where

import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Beside, QuoteLabel, Text)

data DefaultUseToString

foreign import data SetOpenClose :: Symbol -> Symbol -> Config -> Config

foreign import data SetToString :: Type -> Config -> Config

foreign import data Config :: Type

foreign import data MkConfig :: Symbol -> Symbol -> Type -> Config

type DefaultConfig = MkConfig "{" "}" DefaultUseToString

class EvalConfigSpec (spec :: Config) (config :: Config) | spec -> config

instance
  ( EvalConfigSpec tail (MkConfig openX closeX useToString)
  , SymbolIsChar open
  , SymbolIsChar close
  ) =>
  EvalConfigSpec (SetOpenClose open close tail) (MkConfig open close useToString)

instance
  ( EvalConfigSpec tail (MkConfig open close toStringX)
  , SymbolIsChar open
  , SymbolIsChar close
  ) =>
  EvalConfigSpec (SetToString useToString tail) (MkConfig open close useToString)

instance EvalConfigSpec (MkConfig open close useToString) (MkConfig open close useToString)

--------------------------------------------------------------------------------
--- SymbolIsChar
--------------------------------------------------------------------------------

class SymbolIsChar (sym :: Symbol)

instance (Fail (Text "Cannot be empty")) => SymbolIsChar ""

else instance
  ( Sym.Cons head tail sym
  , SymbolIsChar' head tail
  ) =>
  SymbolIsChar sym

class SymbolIsChar' (head :: Symbol) (tail :: Symbol)

instance SymbolIsChar' head ""

else instance
  ( Fail (Beside (QuoteLabel sym) (Text " must be single character"))
  , Sym.Append head tail sym
  ) =>
  SymbolIsChar' head tail