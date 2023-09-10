# purescript-fmt

Format Strings, safely.

```hs
greeting :: String
greeting =
  fmt
    @"""
      Hello, my name is {name}. I live in {city}.
    """
    { name: "Tom", city: "London" }
```