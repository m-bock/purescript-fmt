module Fmt
  ( Config
  , DefaultConfig
  , DefaultUseToString
  , MkConfig
  , SetOpenClose
  , SetToString
  , class EvalConfigSpec
  , class Format
  , class IsAlpha
  , class ParseId
  , class ParseNamed
  , class ReplaceMap
  , class ReplaceMapRL
  , class ToString
  , class ToStringBy
  , class ValidateConfig
  , class ValidateConfigClose
  , class ValidateConfigClose'
  , class ValidateConfigOpen
  , class ValidateConfigOpen'
  , fmt
  , fmtWith
  , format
  , replaceMap
  , replaceMapRL
  , toString
  , toStringBy
  , module Export
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol as Sym
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Data.Boolean (class If)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.Function (type (#)) as Export

--------------------------------------------------------------------------------
--- fmt
--------------------------------------------------------------------------------

fmt
  :: forall @sym replace
   . Format DefaultConfig sym replace
  => replace
  -> String
fmt = format (Proxy :: _ DefaultConfig) (Proxy :: _ sym)

fmtWith
  :: forall @config @sym replace
   . Format config sym replace
  => replace
  -> String
fmtWith = format (Proxy :: _ config) (Proxy :: _ sym)

--------------------------------------------------------------------------------
--- Config
--------------------------------------------------------------------------------

foreign import data Config :: Type
foreign import data MkConfig
  :: Symbol
  -> Symbol
  -> Type
  -> Config

type DefaultConfig = MkConfig "{" "}" DefaultUseToString

--------------------------------------------------------------------------------
-- EvalConfigSpec
--------------------------------------------------------------------------------

foreign import data SetOpenClose :: Symbol -> Symbol -> Config -> Config

foreign import data SetToString :: Type -> Config -> Config

class EvalConfigSpec (spec :: Config) (config :: Config) | spec -> config

instance
  ( EvalConfigSpec tail (MkConfig openX closeX useToString)
  ) =>
  EvalConfigSpec (SetOpenClose open close tail) (MkConfig open close useToString)

instance
  ( EvalConfigSpec tail (MkConfig open close toStringX)
  ) =>
  EvalConfigSpec (SetToString useToString tail) (MkConfig open close useToString)

instance EvalConfigSpec (MkConfig open close useToString) (MkConfig open close useToString)

--------------------------------------------------------------------------------
--- ValidateConfig
--------------------------------------------------------------------------------

class ValidateConfig (config :: Config)

instance
  ( ValidateConfigOpen open
  , ValidateConfigClose close
  ) =>
  ValidateConfig (MkConfig open close useToString)

--------------------------------------------------------------------------------
--- ValidateConfigOpen
--------------------------------------------------------------------------------

class ValidateConfigOpen (sym :: Symbol)

instance (Fail (Text "Open cannot be empty")) => ValidateConfigOpen ""

else instance
  ( Sym.Cons head tail sym
  , ValidateConfigOpen' head tail
  ) =>
  ValidateConfigOpen sym

class ValidateConfigOpen' (head :: Symbol) (tail :: Symbol)

instance ValidateConfigOpen' head ""

else instance
  ( Fail (Text "Open must be single character")
  ) =>
  ValidateConfigOpen' head tail

--------------------------------------------------------------------------------
--- ValidateConfigClose
--------------------------------------------------------------------------------

class ValidateConfigClose (sym :: Symbol)

instance (Fail (Text "Close cannot be empty")) => ValidateConfigClose ""

else instance
  ( Sym.Cons head tail sym
  , ValidateConfigClose' head tail
  ) =>
  ValidateConfigClose sym

class ValidateConfigClose' (head :: Symbol) (tail :: Symbol)

instance ValidateConfigClose' head ""

else instance
  ( Fail (Text "Close must be single character")
  ) =>
  ValidateConfigClose' head tail

--------------------------------------------------------------------------------
--- Fmt
--------------------------------------------------------------------------------

class Format (config :: Config) (sym :: Symbol) (replace :: Type) where
  format :: Proxy config -> Proxy sym -> replace -> String

instance
  ( TypeEquals config (MkConfig open close useToString)
  , ParseNamed config had tail row
  , ValidateConfig config
  , Sym.Cons had tail sym
  , IsSymbol sym
  , ReplaceMap config row
  , IsSymbol open
  , IsSymbol close
  , EvalConfigSpec config' config
  ) =>
  Format config' sym (Record row)
  where
  format _ _ record =
    let
      initStr :: String
      initStr = reflectSymbol (Proxy :: Proxy sym)

      replacements :: Array (String /\ String)
      replacements = replaceMap (Proxy :: Proxy config) record

      replaceConfig =
        { open: reflectSymbol (Proxy :: Proxy open)
        , close: reflectSymbol (Proxy :: Proxy close)
        }
    in
      foldr (replaceNamed replaceConfig) initStr replacements

replaceNamed :: { open :: String, close :: String } -> (String /\ String) -> String -> String
replaceNamed { open, close } (key /\ value) = Str.replaceAll
  (Pattern (open <> key <> close))
  (Replacement value)

--------------------------------------------------------------------------------
--- ToString
--------------------------------------------------------------------------------

class ToStringBy (tok :: Type) (a :: Type) where
  toStringBy :: Proxy tok -> a -> String

data DefaultUseToString

instance (ToString a) => ToStringBy DefaultUseToString a where
  toStringBy _ = toString

class ToString (a :: Type) where
  toString :: a -> String

instance ToString String where
  toString = identity

instance ToString Int where
  toString = show

instance ToString Number where
  toString = show

instance ToString Char where
  toString = show

--------------------------------------------------------------------------------
--- ReplaceMap
--------------------------------------------------------------------------------

class
  ReplaceMap
    (config :: Config)
    (row :: Row Type)
  where
  replaceMap :: Proxy config -> Record row -> Array (String /\ String)

instance
  ( RowToList row rowlist
  , ReplaceMapRL config rowlist row
  ) =>
  ReplaceMap config row
  where
  replaceMap _ = replaceMapRL (Proxy :: Proxy config) (Proxy :: Proxy rowlist)

class
  ReplaceMapRL
    (config :: Config)
    (rowlist :: RowList Type)
    (row :: Row Type)
  where
  replaceMapRL :: Proxy config -> Proxy rowlist -> Record row -> Array (String /\ String)

instance ReplaceMapRL config RL.Nil row where
  replaceMapRL _ _ _ = []

instance
  ( TypeEquals config (MkConfig open close useToString)
  , ReplaceMapRL config tail row
  , ToStringBy useToString value
  , IsSymbol key
  , Row.Cons key value row' row
  ) =>
  ReplaceMapRL config (RL.Cons key value tail) row
  where
  replaceMapRL _ _ record =
    let
      key :: String
      key = reflectSymbol (Proxy :: Proxy key)

      value :: value
      value = Record.get (Proxy :: Proxy key) record

      valueStr :: String
      valueStr = toStringBy (Proxy :: Proxy useToString) value

      tail :: Array (String /\ String)
      tail = replaceMapRL (Proxy :: Proxy config) (Proxy :: Proxy tail) record
    in
      tail `Array.snoc` (key /\ valueStr)

--------------------------------------------------------------------------------
--- ParseNamed
--------------------------------------------------------------------------------

class
  ParseNamed
    (config :: Config)
    (head :: Symbol)
    (tail :: Symbol)
    (replace :: Row Type)
  | config head tail -> replace

instance parseNamed_nil :: ParseNamed config head "" ()

else instance parseNamed_open ::
  ( ParseId config head' tail' tail' replace ""
  , Sym.Cons head' tail' tail
  , TypeEquals config (MkConfig open close useToString)
  ) =>
  ParseNamed (MkConfig open close useToString) open tail replace

else instance parseNamed_other ::
  ( Sym.Cons head' tail' tail
  , ParseNamed config head' tail' replace
  ) =>
  ParseNamed config head tail replace

--------------------------------------------------------------------------------
--- ParseId
--------------------------------------------------------------------------------

class
  ParseId
    (config :: Config)
    (head :: Symbol)
    (tail :: Symbol)
    (backtrack :: Symbol)
    (replace :: Row Type)
    (id :: Symbol)
  | config head tail backtrack -> replace id

instance parseId_nil :: ParseId config head "" backtrack replace id

else instance parseId_finish ::
  ( ParseNamed config head' tail' replace
  , Sym.Cons head' tail' tail
  , Row.Cons id typ replace replace'
  , Row.Nub replace' replace''
  , TypeEquals config (MkConfig open close useToString)
  ) =>
  ParseId (MkConfig open close useToString) close tail backtrack replace'' id

else instance parseId_continueOrFail ::
  ( Sym.Cons head' tail' tail

  , IsAlpha head headIsAlpha
  , If headIsAlpha head' "" a_head
  , If headIsAlpha tail' "" a_tail

  , Sym.Append id head id'
  , ParseId config a_head a_tail backtrack a_replace id'

  , If headIsAlpha "" head' b_head
  , If headIsAlpha "" tail' b_tail
  , ParseNamed config b_head b_tail b_replace

  , If headIsAlpha a_replace b_replace replace
  ) =>
  ParseId config head tail backtrack replace id

--------------------------------------------------------------------------------
--- IsAlpha
--------------------------------------------------------------------------------

class
  IsAlpha
    (sym :: Symbol)
    (bool :: Boolean)
  | sym -> bool

instance IsAlpha "a" True
else instance IsAlpha "b" True
else instance IsAlpha "c" True
else instance IsAlpha "d" True
else instance IsAlpha "e" True
else instance IsAlpha "f" True
else instance IsAlpha "g" True
else instance IsAlpha "h" True
else instance IsAlpha "i" True
else instance IsAlpha "j" True
else instance IsAlpha "k" True
else instance IsAlpha "l" True
else instance IsAlpha "m" True
else instance IsAlpha "n" True
else instance IsAlpha "o" True
else instance IsAlpha "p" True
else instance IsAlpha "q" True
else instance IsAlpha "r" True
else instance IsAlpha "s" True
else instance IsAlpha "t" True
else instance IsAlpha "u" True
else instance IsAlpha "v" True
else instance IsAlpha "w" True
else instance IsAlpha "x" True
else instance IsAlpha "y" True
else instance IsAlpha "z" True
else instance IsAlpha other False
