## Imports


```hs
module DemoMore where

import Prelude

import Data.String as Str
import Fmt (fmt, fmtWith, type (#))
import Fmt as Fmt
```

## Sample without config


```hs
greeting1 :: String
greeting1 =
  fmt
    @"""
      Hello, my name is {name}.
      I live in {city}.
      I am {age} years old.
    """
    { name: "Tom"
    , city: "London"
    , age: 42
    }
```

By default you can only use a limited set of types in the replacements:
  - `String`
  - `Int`
  - `Number`
  - Char


## Sample with simple config

In this sample we're overriding the default config to use "<" and ">" as
open/close symbols.


```hs
type MySimpleCfg =
  Fmt.DefaultConfig
    # Fmt.SetOpenClose "<" ">"

greeting2 :: String
greeting2 =
  fmtWith
    @MySimpleCfg
    @"""
      Hello, my name is <name>. I live in <city>. I am <age> years old.
    """
    { name: "Tom"
    , city: "London"
    , age: 28
    }
```

## Sample with advanced config

In this sample we're extending the simple config to use a custom typeclass
for converting values to strings.



```hs
type MyAdvancedCfg =
  Fmt.DefaultConfig
    # Fmt.SetOpenClose "<" ">"
    # Fmt.SetToString UseMyToString

data UseMyToString

instance
  ( MyToString a sym
  ) =>
  Fmt.ToStringBy UseMyToString a sym where
  toStringBy _ = myToString

class MyToString a (sym :: Symbol) | a -> sym where
  myToString :: a -> String

instance MyToString Int "int" where
  myToString = show

instance MyToString String "string" where
  myToString = identity

instance MyToString (Array String) "array_string" where
  myToString = Str.joinWith ", "

greeting3 :: String
greeting3 =
  fmtWith
    @MyAdvancedCfg
    @"""
      Hello, my name is <name>. I live in <city>.
      My hobbies are: <hobbies>
    """
    { name: "Tom"
    , city: "London"
    , hobbies: [ "football", "basketball", "swimming" ]
    }
```

## Optionally annotate replacements with type info

Finally you can optionally annotate replacements with type info.


```hs
greeting4 :: String
greeting4 =
  fmt
    @"""
      Hello, my name is {name@string}.
      I am {age@int} years old.
    """
    { name: "Tom"
    , age: 42
    }
```