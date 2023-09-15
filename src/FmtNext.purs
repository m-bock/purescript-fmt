-- @inline export parseCons(..).parse arity=3
-- @inline export parseStart(..).parse arity=3
-- @inline export parseNil(..).parse arity=3

-- @inline export parseIdCons(..).parseId always
-- @inline export parseIdStart(..).parseId always
-- @inline export parseIdEnd(..).parseId always
-- @inline export parseIdNil(..).parseId always
-- @inline export parseIdAt(..).parseId always

-- @inline export parseTypeIdCons(..).parseTypeId always
-- @inline export parseTypeIdEnd(..).parseTypeId always
-- @inline export parseTypeIdNil(..).parseTypeId always

-- @inline export formatCons(..).format arity=1
-- @inline export formatNil(..).format arity=1

-- @inline export fmt arity=1
-- @inline export fmtWith arity=1

module FmtNext
  ( module Export
  , fmt
  , fmtWith
  , class Parse
  , class ParseId
  , class ParseTypeId
  , class ToString
  , class ToStringBy
  , parse
  , parseId
  , parseTypeId
  , toString
  , toStringBy
  , EOF
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Fmt.Config (class EvalConfigSpec, Config, DefaultConfig, DefaultUseToString, MkConfig)
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.Function (type (#)) as Export
import Fmt.Config (Config, DefaultConfig, SetOpenClose, SetToString) as Export

fmt
  :: forall @sym sym' head tail replace
   . Parse DefaultConfig head tail replace
  => Sym.Append sym EOF sym'
  => Sym.Cons head tail sym'
  => replace
  -> String
fmt = fmtWith @DefaultConfig @sym

fmtWith
  :: forall @config @sym config' sym' head tail replace
   . Parse config' head tail replace
  => Sym.Append sym EOF sym'
  => Sym.Cons head tail sym'
  => EvalConfigSpec config config'
  => replace
  -> String
fmtWith r = parse
  (Proxy :: Proxy config')
  (Proxy :: _ head)
  (Proxy :: _ tail)
  r
  ""

type EOF = "$"

--------------------------------------------------------------------------------
--- Parse
--------------------------------------------------------------------------------

class
  Parse
    (config :: Config)
    (head :: Symbol)
    (tail :: Symbol)
    (replace :: Type)
  where
  parse
    :: Proxy config
    -> Proxy head
    -> Proxy tail
    -> replace
    -> String
    -> String

instance parseNil :: Parse config EOF "" replace where
  parse _ _ _ _ x = x

else instance parseStart ::
  ( Sym.Cons head' tail' tail
  , ParseId config head' tail' "" (Record replace)
  , TypeEquals config (MkConfig open close useToString)
  ) =>
  Parse (MkConfig open close useToString) open tail (Record replace)
  where
  parse _ _ _ repl str =
    parseId
      (Proxy :: Proxy config)
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      (Proxy :: Proxy "")
      repl
      str

else instance parseCons ::
  ( Sym.Cons head' tail' tail
  , Parse config head' tail' (Record replace)
  , IsSymbol head
  ) =>
  Parse config head tail (Record replace)
  where
  parse _ _ _ repl str =
    parse
      (Proxy :: Proxy config)
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      repl
      (str <> reflectSymbol (Proxy :: Proxy head))

--------------------------------------------------------------------------------
--- ParseId
--------------------------------------------------------------------------------

class
  ParseId
    (config :: Config)
    (head :: Symbol)
    (tail :: Symbol)
    (id :: Symbol)
    (replace :: Type)
  where
  parseId
    :: Proxy config
    -> Proxy head
    -> Proxy tail
    -> Proxy id
    -> replace
    -> String
    -> String

instance parseIdNil :: ParseId config EOF "" id replace where
  parseId _ _ _ _ _ x = x

else instance parseIdAt ::
  ( Sym.Cons head' tail' tail
  , ParseTypeId config head' tail' id "" (Record replace)
  ) =>
  ParseId config "@" tail id (Record replace)
  where
  parseId _ _ _ _ repl str = parseTypeId
    (Proxy :: Proxy config)
    (Proxy :: Proxy head')
    (Proxy :: Proxy tail')
    (Proxy :: Proxy id)
    (Proxy :: Proxy "")
    repl
    str

else instance parseIdEnd ::
  ( Sym.Cons head' tail' tail
  , Parse config head' tail' (Record replace)
  , IsSymbol id
  , Row.Cons id a replace' replace
  , ToStringBy useToString a typSym
  , TypeEquals config (MkConfig open close useToString)
  ) =>
  ParseId (MkConfig open close useToString) close tail id (Record replace)
  where
  parseId _ _ _ _ repl str =
    let
      val :: a
      val = Record.get (Proxy :: Proxy id) repl

      valStr :: String
      valStr = toStringBy (Proxy :: Proxy useToString) val
    in
      parse
        (Proxy :: Proxy config)
        (Proxy :: Proxy head')
        (Proxy :: Proxy tail')
        repl
        (str <> valStr)

else instance parseIdCons ::
  ( Sym.Cons head' tail' tail
  , ParseId config head' tail' id' (Record replace)
  , Sym.Append id head id'
  ) =>
  ParseId config head tail id (Record replace)
  where
  parseId _ _ _ _ repl str =
    parseId
      (Proxy :: Proxy config)
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      (Proxy :: Proxy id')
      repl
      str

--------------------------------------------------------------------------------
--- ParseTypeId
--------------------------------------------------------------------------------

class
  ParseTypeId
    (config :: Config)
    (head :: Symbol)
    (tail :: Symbol)
    (id :: Symbol)
    (typeId :: Symbol)
    (replace :: Type)
  where
  parseTypeId
    :: Proxy config
    -> Proxy head
    -> Proxy tail
    -> Proxy id
    -> Proxy typeId
    -> replace
    -> String
    -> String

instance parseTypeIdNil :: ParseTypeId config EOF "" id typeId replace where
  parseTypeId _ _ _ _ _ _ x = x

else instance parseTypeIdEnd ::
  ( Sym.Cons head' tail' tail
  , Parse config head' tail' (Record replace)
  , IsSymbol id
  , Row.Cons id a replace' replace
  , ToStringBy useToString a typeId
  , TypeEquals config (MkConfig open close useToString)
  ) =>
  ParseTypeId (MkConfig open close useToString) close tail id typeId (Record replace)
  where
  parseTypeId _ _ _ _ _ repl str =
    let
      val :: a
      val = Record.get (Proxy :: Proxy id) repl

      valStr :: String
      valStr = toStringBy (Proxy :: Proxy useToString) val
    in
      parse
        (Proxy :: Proxy config)
        (Proxy :: Proxy head')
        (Proxy :: Proxy tail')
        repl
        (str <> valStr)

else instance parseTypeIdCons ::
  ( Sym.Cons head' tail' tail
  , ParseTypeId config head' tail' id typeId' (Record replace)
  , Sym.Append typeId head typeId'
  ) =>
  ParseTypeId config head tail id typeId (Record replace)
  where
  parseTypeId _ _ _ _ _ repl str =
    parseTypeId
      (Proxy :: Proxy config)
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      (Proxy :: Proxy id)
      (Proxy :: Proxy typeId')
      repl
      str

--------------------------------------------------------------------------------
--- ToString
--------------------------------------------------------------------------------

class
  ToStringBy (tok :: Type) (a :: Type) (sym :: Symbol)
  | tok a -> sym
  where
  toStringBy :: Proxy tok -> a -> String

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
