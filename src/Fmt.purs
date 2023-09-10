module Fmt
  ( Config
  , DefaultConfig
  , DefaultUseToString
  , MkConfig
  , SetOpenClose
  , SetToString
  , class Format
  , class IsValidChar
  , class ParseId
  , class ParseNamed
  , class ReplaceMap
  , class ReplaceMapRL
  , class ToString
  , class ToStringBy
  , class SymbolIsChar
  , class SymbolIsChar'
  , class EvalConfigSpec
  , class ParseTypeId
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
import Prim.TypeError (class Fail, Beside, QuoteLabel, Text)
import Record as Record
import Type.Data.Boolean (class If)
import Type.Equality (class TypeEquals)
import Type.Function (type (#)) as Export
import Type.Proxy (Proxy(..))

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

--------------------------------------------------------------------------------
--- Fmt
--------------------------------------------------------------------------------

class Format (config :: Config) (sym :: Symbol) (replace :: Type) where
  format :: Proxy config -> Proxy sym -> replace -> String

instance
  ( TypeEquals config (MkConfig open close useToString)
  , ParseNamed config had tail row
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

class
  ToStringBy (tok :: Type) (a :: Type) (sym :: Symbol)
  | tok a -> sym
  where
  toStringBy :: Proxy tok -> a -> String

data DefaultUseToString

instance (ToString a sym) => ToStringBy DefaultUseToString a sym where
  toStringBy _ = toString

class
  ToString (a :: Type) (sym :: Symbol)
  | a -> sym
  where
  toString :: a -> String

instance ToString String "string" where
  toString = identity

instance ToString Int "int" where
  toString = show

instance ToString Number "number" where
  toString = show

instance ToString Char "char" where
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
  , ToStringBy useToString value typSym
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

instance parseId_finishEnd ::
  ( ParseNamed config "" "" replace
  , Row.Cons id typ replace replace'
  , Row.Nub replace' replace''
  ) =>
  ParseId (MkConfig open close useToString) close "" backtrack replace'' id


else instance parseId_nil ::
  ( ParseNamed config head' tail' replace
  , Sym.Cons head' tail' backtrack
  ) =>
  ParseId config head "" backtrack replace id

else instance parseId_finish ::
  ( ParseNamed config head' tail' replace
  , Sym.Cons head' tail' tail
  , Row.Cons id typ replace replace'
  , Row.Nub replace' replace''
  , TypeEquals config (MkConfig open close useToString)
  ) =>
  ParseId (MkConfig open close useToString) close tail backtrack replace'' id

else instance parseId_startTypeId ::
  ( ParseTypeId config head' tail' backtrack replace id ""
  , Sym.Cons head' tail' tail
  ) =>
  ParseId config "@" tail backtrack replace id

else instance parseId_continueOrFail ::
  ( Sym.Cons head' tail' tail

  , IsValidChar head headIsValidChar
  , If headIsValidChar head' "" a_head
  , If headIsValidChar tail' "" a_tail

  , Sym.Append id head id'
  , ParseId config a_head a_tail backtrack a_replace id'

  , Sym.Cons backtrackHead' backtrackTail' backtrack
  , If headIsValidChar "" backtrackHead' b_head
  , If headIsValidChar "" backtrackTail' b_tail
  , ParseNamed config b_head b_tail b_replace

  , If headIsValidChar a_replace b_replace replace
  ) =>
  ParseId config head tail backtrack replace id

--------------------------------------------------------------------------------
--- ParseTypeId
--------------------------------------------------------------------------------

class
  ParseTypeId
    (config :: Config)
    (head :: Symbol)
    (tail :: Symbol)
    (backtrack :: Symbol)
    (replace :: Row Type)
    (id :: Symbol)
    (typeId :: Symbol)

instance parseTypeId_nil :: ParseTypeId config head "" backtrack replace id typeId

else instance parseTypeId_finish ::
  ( ParseNamed config head' tail' replace
  , Sym.Cons head' tail' tail
  , Row.Cons id typ replace replace'
  , Row.Nub replace' replace''
  , TypeEquals config (MkConfig open close useToString)
  , ToStringBy useToString typ typeId
  ) =>
  ParseTypeId (MkConfig open close useToString) close tail backtrack replace'' id typeId

else instance parseTypeId_continueOrFail ::
  ( Sym.Cons head' tail' tail

  , IsValidChar head headIsValidChar
  , If headIsValidChar head' "" a_head
  , If headIsValidChar tail' "" a_tail

  , Sym.Append typeId head typeId'
  , ParseTypeId config a_head a_tail backtrack a_replace id typeId'

  , If headIsValidChar "" head' b_head
  , If headIsValidChar "" tail' b_tail
  , ParseNamed config b_head b_tail b_replace

  , If headIsValidChar a_replace b_replace replace
  ) =>
  ParseTypeId config head tail backtrack replace id typeId

--------------------------------------------------------------------------------
--- IsValidChar
--------------------------------------------------------------------------------

class
  IsValidChar
    (sym :: Symbol)
    (bool :: Boolean)
  | sym -> bool

instance IsValidChar "a" True
else instance IsValidChar "b" True
else instance IsValidChar "c" True
else instance IsValidChar "d" True
else instance IsValidChar "e" True
else instance IsValidChar "f" True
else instance IsValidChar "g" True
else instance IsValidChar "h" True
else instance IsValidChar "i" True
else instance IsValidChar "j" True
else instance IsValidChar "k" True
else instance IsValidChar "l" True
else instance IsValidChar "m" True
else instance IsValidChar "n" True
else instance IsValidChar "o" True
else instance IsValidChar "p" True
else instance IsValidChar "q" True
else instance IsValidChar "r" True
else instance IsValidChar "s" True
else instance IsValidChar "t" True
else instance IsValidChar "u" True
else instance IsValidChar "v" True
else instance IsValidChar "w" True
else instance IsValidChar "x" True
else instance IsValidChar "y" True
else instance IsValidChar "z" True
else instance IsValidChar "A" True
else instance IsValidChar "B" True
else instance IsValidChar "C" True
else instance IsValidChar "D" True
else instance IsValidChar "E" True
else instance IsValidChar "F" True
else instance IsValidChar "G" True
else instance IsValidChar "H" True
else instance IsValidChar "I" True
else instance IsValidChar "J" True
else instance IsValidChar "K" True
else instance IsValidChar "L" True
else instance IsValidChar "M" True
else instance IsValidChar "N" True
else instance IsValidChar "O" True
else instance IsValidChar "P" True
else instance IsValidChar "Q" True
else instance IsValidChar "R" True
else instance IsValidChar "S" True
else instance IsValidChar "T" True
else instance IsValidChar "U" True
else instance IsValidChar "V" True
else instance IsValidChar "W" True
else instance IsValidChar "X" True
else instance IsValidChar "Y" True
else instance IsValidChar "Z" True
else instance IsValidChar "1" True
else instance IsValidChar "2" True
else instance IsValidChar "3" True
else instance IsValidChar "4" True
else instance IsValidChar "5" True
else instance IsValidChar "6" True
else instance IsValidChar "7" True
else instance IsValidChar "8" True
else instance IsValidChar "9" True
else instance IsValidChar "0" True
else instance IsValidChar "'" True
else instance IsValidChar "_" True
else instance IsValidChar other False
