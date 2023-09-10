module Use where

import Prelude

import Data.String as Str
import Fmt (fmt, fmtWith, type (#))
import Fmt as Fmt

greeting :: String
greeting =
  fmt
    @"""
      Hello, my name is {name}. I live in {city}.
    """
    { name: "Tom", city: "London" }

type Cfg2 =
  Fmt.DefaultConfig
    # Fmt.SetOpenClose "<" ">"

greeting2 :: String
greeting2 =
  fmtWith
    @Cfg2
    @"""
      Hello, my name is <name>. I live in <city>.
    """
    { name: "Tom"
    , city: "London"
    }

type Cfg3 =
  Fmt.DefaultConfig
    # Fmt.SetToString UseMyToString

greeting3 :: String
greeting3 =
  fmtWith
    @Cfg3
    @"""
      Hello, my name is {name}. I live in {city}.
      My hobbies are: {hobbies}
    """
    { name: "Tom"
    , city: "London"
    , hobbies: [ "football", "basketball", "swimming" ]
    }

data UseMyToString

instance (MyToString a) => Fmt.ToStringBy UseMyToString a where
  toStringBy _ = myToString

class MyToString a where
  myToString :: a -> String

instance MyToString Int where
  myToString = show

instance MyToString String where
  myToString = identity

instance MyToString (Array String) where
  myToString = Str.joinWith ", "