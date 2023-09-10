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
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

greeting :: String
greeting =
  fmt
    @"""
      Hello, my name is {name}. I live in {city}.   
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    
      Hello, my name is {name}. I live in {city}.    

    """
    { name: "Tom", city: "London" }

--------------------------------------------------------------------------------
--- fmt
--------------------------------------------------------------------------------

fmt
  :: forall @sym replace
   . Format DefaultConfig sym replace
  => replace
  -> String
fmt = format (Proxy :: _ DefaultConfig) (Proxy :: _ sym)

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
--- Config
--------------------------------------------------------------------------------

foreign import data ConfigOpen :: Type
foreign import data MkConfigOpen :: Symbol -> ConfigOpen

foreign import data MkConfigClose :: Symbol -> ConfigClose
foreign import data ConfigClose :: Type

foreign import data ConfigToString :: Type
foreign import data MkConfigToString :: Type -> ConfigToString

foreign import data Config :: Type
foreign import data MkConfig :: ConfigOpen -> ConfigClose -> ConfigToString -> Config

type DefaultConfig = MkConfig
  (MkConfigOpen "{")
  (MkConfigClose "}")
  (MkConfigToString DefaultToString)

--------------------------------------------------------------------------------
--- Fmt
--------------------------------------------------------------------------------

class Format (config :: Config) (sym :: Symbol) (replace :: Type) where
  format :: Proxy config -> Proxy sym -> replace -> String

instance
  ( SymToList sym tokens
  , ParseNamed config tokens row
  , IsSymbol sym
  , ReplaceMap config row
  , TypeEquals config (MkConfig (MkConfigOpen open) (MkConfigClose close) configToString)
  , IsSymbol open
  , IsSymbol close
  ) =>
  Format config sym (Record row)
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

data DefaultToString

instance (ToString a) => ToStringBy DefaultToString a where
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
  ( ReplaceMapRL config tail row
  , ToStringBy configToString value
  , IsSymbol key
  , Row.Cons key value row' row
  , TypeEquals config (MkConfig configOpen configClose (MkConfigToString configToString))
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
      valueStr = toStringBy (Proxy :: Proxy configToString) value

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
    (tokens :: List' Symbol)
    (replace :: Row Type)
  | tokens -> replace

instance ParseNamed config Nil' ()

else instance
  ( TypeEquals config (MkConfig (MkConfigOpen open) (MkConfigClose close) configToString)
  , SymToList open openTokens
  , ParseOpen config openTokens tokens replace
  ) =>
  ParseNamed config tokens replace

--------------------------------------------------------------------------------
--- ParseSym
--------------------------------------------------------------------------------

class ParseOpen (config :: Config) (open :: List' Symbol) (tokens :: List' Symbol) (replace :: Row Type)

instance
  ( ParseId config tokens tokens replace ""
  ) =>
  ParseOpen config Nil' tokens replace

instance
  ( ParseOpen config tailSym tailTokens replace
  ) =>
  ParseOpen config (head :> tailSym) (head :> tailTokens) replace

else instance
  ( ParseNamed config tailTokens replace
  ) =>
  ParseOpen config (head1 :> tailSym) (head2 :> tailTokens) replace

---

testParseSym :: forall sym tokensIn tokensOut. Proxy sym -> Proxy tokensIn -> Proxy tokensOut -> Unit
testParseSym _ _ _ = unit

t1 :: Unit
t1 = testParseSym
  (Proxy :: _ ("a" :> "b" :> Nil'))
  (Proxy :: _ ("a" :> "b" :> "c" :> Nil'))
  (Proxy :: _ ("c" :> Nil'))

--------------------------------------------------------------------------------
--- ParseId
--------------------------------------------------------------------------------

class
  ParseId
    (config :: Config)
    (tokens :: List' Symbol)
    (backtrack :: List' Symbol)
    (replace :: Row Type)
    (id :: Symbol)

instance ParseId config Nil' tokens replace id

instance
  ( ParseNamed config tokens replace
  , Row.Cons id typ replace replace'
  , Row.Nub replace' replace''
  , TypeEquals config (MkConfig (MkConfigOpen open) (MkConfigClose close) configToString)
  ) =>
  ParseId (MkConfig (MkConfigOpen open) (MkConfigClose close) configToString) (close :> tokens) backtrack replace'' id

else instance
  ( IsAlpha head bool
  , If bool tokens Nil' a_xs

  , Sym.Append id head id'
  , ParseId config a_xs backtrack a_replace id'

  , If bool Nil' tokens b_xs
  , ParseNamed config b_xs b_replace

  , If bool a_replace b_replace replace
  ) =>
  ParseId config (head :> tokens) backtrack replace id

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
