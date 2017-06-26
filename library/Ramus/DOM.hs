module Ramus.DOM () where

import Ramus.Signal
import Ramus.Time

import GHCJS.Foreign.Callback

data CoordinatePair = CoordinatePair { x :: Int, y :: Int }
data DimensionPair  = DimensionPair { w :: Int, h :: Int }

foreign import javascript unsafe "window.addEventListener('keydown', function(e){\
                                    if(e.keyCode === $1) $2(true);\
                                 });\
                                 window.addEventListener('keyup', function(e){\
                                    if(e.keyCode === $1) $2(false);\
                                 });"
    js_keyPressed :: Int -> Callback (Bool -> IO ()) -> IO ()

foreign import javascript unsafe "window.addEventListener('mousedown', function(e){\
                                    if(e.keyCode === $1) $2(true);\
                                 });\
                                 window.addEventListener('mouseup', function(e){\
                                    if(e.keyCode === $1) $2(false);\
                                 });"
    js_mouseButton :: Int -> Callback (Bool -> IO ()) -> IO ()


-- |Creates a signal which will be `true` when the key matching the given key
-- |code is pressed, and `false` when it's released.
keyPressed :: Int -> IO (Signal Bool)
keyPressed keyCode = do
    let out = constant False
    cb <- asyncCallback1 (set out)
    js_keyPressed keyCode cb
    return out

-- |Creates a signal which will be `true` when the given mouse button is
-- |pressed, and `false` when it's released.
mouseButton :: Int -> IO (Signal Bool)
mouseButton button = do
    let out = constant False
    cb <- asyncCallback1 (set out)
    js_mouseButton button cb
    return out

data Touch = Touch
  { id :: String
  , screenX :: Int
  , screenY :: Int

  , clientX :: Int
  , clientY :: Int

  , pageX :: Int
  , pageY :: Int

  , radiusX :: Int
  , radiusY :: Int

  , rotationAngle :: Float
  , force :: Float
  }

-- |A signal containing the current state of the touch device, as described by
-- |the `Touch` record type.
touch :: IO (Signal [Touch])
touch = undefined

-- |A signal which will be `true` when at least one finger is touching the
-- |touch device, and `false` otherwise.
tap :: IO (Signal Bool)
tap = undefined

-- |A signal containing the current mouse position.
mousePos :: IO (Signal CoordinatePair)
mousePos = undefined

-- |A signal which yields the current time, as determined by `now`, on every
-- |animation frame (see [https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame]).
animationFrame :: IO (Signal Time)
animationFrame = undefined

-- |A signal which contains the document window's current width and height.
windowDimensions :: IO (Signal DimensionPair)
windowDimensions = undefined constant
