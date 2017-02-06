module Ramus.Internal where

import Data.IORef
import System.IO.Unsafe
import Control.Monad
import Data.Monoid

data Signal a = Signal
  { get :: a
  , set :: a -> IO ()
  , subscribe :: (a -> IO ()) -> IO ()
  }

unsafeRef :: a -> IORef a
unsafeRef = unsafePerformIO . newIORef

unsafeRead :: IORef a -> a
unsafeRead = unsafePerformIO . readIORef

make :: a -> Signal a
make initial = unsafePerformIO $ do
  subs <- newIORef [] :: IO (IORef [a -> IO()])
  val  <- newIORef initial
  let _get = unsafeRead val
  let _set newval = do
        writeIORef val newval
        forM_ (unsafeRead subs) $ \sub ->
          sub newval
  let _subscribe sub = do
        currentSubs <- readIORef subs
        _val <- readIORef val
        writeIORef subs $ currentSubs <> [sub]
        sub _val
  return Signal
    { get = _get
    , set = _set
    , subscribe = _subscribe
    }
