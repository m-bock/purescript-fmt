module DemoPerf where

import Fmt (fmt)

{-
# Runtime performance

If you use [purs-backend-es](https://github.com/aristanetworks/purescript-backend-optimizer)
you'll get the following runtime code:


## Static replacements
-}

greeting1 :: String
greeting1 =
  fmt
    @"Hello, my name is {name}. I live in {city}."
    { name: "John"
    , city: "London"
    }

{-

```js
const greeting1 = "Hello, my name is John. I live in London.";
```

## Dynamic replacements
-}

greeting2 :: { name :: String, city :: String } -> String
greeting2 fields =
  fmt
    @"Hello, my name is {name}. I live in {city}."
    fields
{-

```js
const greeting2 = (fields) =>
  "Hello, my name is " + fields.name + ". I live in " + fields.city + ".";
```

Note that currently you'll lose the optimization if you don't explicitly apply
the `fields` record in example 2. This will hopefully be fixed in the future.
-}