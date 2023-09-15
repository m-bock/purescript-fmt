-- @inline export parseCons(..).parse arity=3
-- @inline export parseStart(..).parse arity=3
-- @inline export parseNil(..).parse arity=3

-- @inline export parseIdCons(..).parseId always
-- @inline export parseIdStart(..).parseId always
-- @inline export parseIdEnd(..).parseId always

-- @inline export formatCons(..).format arity=1
-- @inline export formatNil(..).format arity=1

-- @inline export fmt arity=1

module FmtNext where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))

fmt
  :: forall @sym sym' head tail replace
   . (Parse head tail replace)
  => (Sym.Append sym EOF sym')
  => (Sym.Cons head tail sym')
  => replace
  -> String
fmt r = parse
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
    (head :: Symbol)
    (tail :: Symbol)
    (replace :: Type)
  where
  parse :: Proxy head -> Proxy tail -> replace -> String -> String

instance parseNil :: Parse EOF "" replace where
  parse _ _ _ x = x

else instance parseStart ::
  ( Sym.Cons head' tail' tail
  , ParseId head' tail' "" (Record replace)
  ) =>
  Parse "{" tail (Record replace)
  where
  parse _ _ repl str =
    parseId
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      (Proxy :: Proxy "")
      repl
      str

else instance parseCons ::
  ( Sym.Cons head' tail' tail
  , Parse head' tail' (Record replace)

  , IsSymbol head
  ) =>
  Parse head tail (Record replace)
  where
  parse _ _ repl str =
    parse
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      repl
      (str <> reflectSymbol (Proxy :: Proxy head))

--------------------------------------------------------------------------------
--- ParseId
--------------------------------------------------------------------------------

class
  ParseId
    (head :: Symbol)
    (tail :: Symbol)
    (id :: Symbol)
    (replace :: Type)
  where
  parseId :: Proxy head -> Proxy tail -> Proxy id -> replace -> String -> String

instance parseIdNil :: ParseId EOF "" id replace where
  parseId _ _ _ _ x = x

else instance parseIdEnd ::
  ( Sym.Cons head' tail' tail
  , Parse head' tail' (Record replace)
  , IsSymbol id
  , Row.Cons id String replace' replace
  ) =>
  ParseId "}" tail id (Record replace)
  where
  parseId _ _ _ repl str =
    parse
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      repl
      (str <> Record.get (Proxy :: Proxy id) repl)

else instance parseIdCons ::
  ( Sym.Cons head' tail' tail
  , ParseId head' tail' id' (Record replace)
  , Sym.Append id head id'
  ) =>
  ParseId head tail id (Record replace)
  where
  parseId _ _ _ repl str =
    parseId
      (Proxy :: Proxy head')
      (Proxy :: Proxy tail')
      (Proxy :: Proxy id')
      repl
      str

------------------------------------------------------------

greeting8 :: String -> String -> String
greeting8 xy ab = fmt @"jkhjkhaa{xy}bb{ab}u" { xy, ab: "l" }

greeting9 :: String -> String -> String
greeting9 xy ab = fmt @"jkhaa{xy}bb{ab}u" { xy, ab: "l" }

