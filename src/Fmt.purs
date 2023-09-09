module Fmt where

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
import Record as Record
import Type.Data.Boolean (class If)
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

t1 :: String
t1 = fmt @"Hello, my name is {name}. I live in {city}."
  { name: "Tom", city: "London" }

--------------------------------------------------------------------------------
--- fmt
--------------------------------------------------------------------------------

fmt :: forall @sym replace. Format sym replace => replace -> String
fmt = format (Proxy :: _ sym)

--------------------------------------------------------------------------------
--- SymToList
--------------------------------------------------------------------------------

class SymToList (sym :: Symbol) (xs :: List' Symbol) | sym -> xs

instance SymToList "" Nil'

else instance
  ( Sym.Cons head tail sym
  , SymToList tail xs
  ) =>
  SymToList sym (head :> xs)

--------------------------------------------------------------------------------
--- Fmt
--------------------------------------------------------------------------------

class Format (sym :: Symbol) (replace :: Type) where
  format :: Proxy sym -> replace -> String

instance
  ( SymToList sym tokens
  , ParseNamed tokens row
  , IsSymbol sym
  , ReplaceMap row
  ) =>
  Format sym (Record row)
  where
  format _ record =
    let
      initStr :: String
      initStr = reflectSymbol (Proxy :: Proxy sym)

      replacements :: Array (String /\ String)
      replacements = replaceMap record
    in
      foldr replaceNamed initStr replacements

replaceNamed :: (String /\ String) -> String -> String
replaceNamed (key /\ value) = Str.replace
  (Pattern ("{" <> key <> "}"))
  (Replacement value)

--------------------------------------------------------------------------------
--- ToString
--------------------------------------------------------------------------------

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

class ReplaceMap row where
  replaceMap :: Record row -> Array (String /\ String)

instance
  ( RowToList row rowlist
  , ReplaceMapRL rowlist row
  ) =>
  ReplaceMap row where
  replaceMap = replaceMapRL (Proxy :: Proxy rowlist)

class
  ReplaceMapRL
    (rowlist :: RowList Type)
    (row :: Row Type)
  where
  replaceMapRL :: Proxy rowlist -> Record row -> Array (String /\ String)

instance ReplaceMapRL RL.Nil row where
  replaceMapRL _ _ = []

instance
  ( ReplaceMapRL tail row
  , ToString value
  , IsSymbol key
  , Row.Cons key value row' row
  ) =>
  ReplaceMapRL (RL.Cons key value tail) row
  where
  replaceMapRL _ record =
    let
      key :: String
      key = reflectSymbol (Proxy :: Proxy key)

      value :: value
      value = Record.get (Proxy :: Proxy key) record

      valueStr :: String
      valueStr = toString value

      tail :: Array (String /\ String)
      tail = replaceMapRL (Proxy :: Proxy tail) record
    in
      tail `Array.snoc` (key /\ valueStr)

--------------------------------------------------------------------------------
--- ParseNamed
--------------------------------------------------------------------------------

class
  ParseNamed
    (tokens :: List' Symbol)
    (replace :: Row Type)
  | tokens -> replace

instance ParseNamed Nil' ()

instance (ParseId tokens tokens replace "") => ParseNamed ("{" :> tokens) replace

else instance (ParseNamed tokens replace) => ParseNamed (head :> tokens) replace

--------------------------------------------------------------------------------
--- ParseId
--------------------------------------------------------------------------------

class
  ParseId
    (tokens :: List' Symbol)
    (backtrack :: List' Symbol)
    (replace :: Row Type)
    (id :: Symbol)

instance ParseId Nil' tokens replace id

instance
  ( ParseNamed tokens replace
  , Row.Cons id typ replace replace'
  , Row.Nub replace' replace''
  ) =>
  ParseId ("}" :> tokens) backtrack replace'' id

else instance
  ( IsAlpha head bool
  , If bool tokens Nil' a_xs

  , Sym.Append id head id'
  , ParseId a_xs backtrack a_replace id'

  , If bool Nil' tokens b_xs
  , ParseNamed b_xs b_replace

  , If bool a_replace b_replace replace
  ) =>
  ParseId (head :> tokens) backtrack replace id

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
else instance IsAlpha c False
