{-
# Runtime performance

If you use [purs-backend-es](https://github.com/aristanetworks/purescript-backend-optimizer)
you'll get optimized code for free. This is a simple example of how it works.

For the code in this file, you need those imports:
-}

module DemoPerf where

import Fmt (fmt)

{-

## Static replacements

A simple replacement of strings known at compile time...
-}

greeting1 :: String
greeting1 =
  fmt
    @"Hello, my name is {name}. I live in {city}."
    { name: "John"
    , city: "London"
    }

{-

...will compile to a plain JS string:
```js
const greeting1 = "Hello, my name is John. I live in London.";
```

## Dynamic replacements

Replacing strings with values not known at compile time...
-}

greeting2 :: { name :: String, city :: String } -> String
greeting2 =
  fmt
    @"Hello, my name is {name}. I live in {city}."
{-

...will compile to simple string concatenation:
```js
const greeting2 = (fields) =>
  "Hello, my name is " + fields.name + ". I live in " + fields.city + ".";
```
-}