# purescript-fmt

Format strings, safely.

![Latest release](logo.svg)

## Sample


```hs
module Demo where

import Fmt (fmt)

greeting :: String
greeting =
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

You can [check out more examples here](./Samples.md).

## Features

- Compile-time format string validation
- Easily extensible with custom formatter type class
- Configurable placeholder syntax
