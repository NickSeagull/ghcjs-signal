# Ramus

Ramus is a lightweight FRP-like library heavily inspired by the Elm Signal implementation,
in fact, is a direct port of the [purescript-signal](https://github.com/bodil/purescript-signal) library,
but in Haskell.
Where possible and sensible, it tries to maintain API equivalence with Elm.

See [the Elm documentation](http://elm-lang.org:1234/guide/reactivity#signals) for details on usage and principles.

## Haskell Usage Patterns

Haskell depends on `IO` to manage side effects, where Elm's runtime generally manages them for you.
`purescript-signal` provides the `Signal.runSignal` function for running effectful signals.

```haskell
module Main where

import Signal

hello :: Signal String
hello = constant "Hello Joe!"

helloEffect :: Signal (IO ())
helloEffect = hello ~> print

main :: IO ()
main = runSignal helloEffect
```

This simple example takes a constant signal which contains the string `"Hello Joe!"`
and maps it over the `print` function, which has the type `(Show a) => a -> IO()`, thus taking the `String`
content of the signal and turning it into an effect which logs the provided string to the user's console.

This gives us a `Signal (IO ())`. We use `runSignal` to take the signal of effects and run each effect
in turn—in our case, just the one effect which prints `"Hello Joe!"` to the console.

## API Documentation

* TODO: Add to hackage

## Usage Examples

* TODO
