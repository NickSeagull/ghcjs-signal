module Signal where

import Prelude hiding (filter)

import Control.Applicative
import Data.Functor
import Data.Semigroup
import Data.Foldable
import Data.Maybe
import Data.IORef
import System.IO.Unsafe

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
         




-- |Creates a signal with a constant value.
constant :: a -> Signal a
constant = make

-- |Merge two signals, returning a new signal which will yield a value
-- |whenever either of the input signals yield. Its initial value will be
-- |that of the first signal.
merge :: Signal a -> Signal a -> Signal a
merge sig1 sig2 = unsafePerformIO $ do
  let out = constant $ get sig1
  sig2 `subscribe` set out
  sig1 `subscribe` set out
  return out

-- |Merge all signals inside a `Foldable`, returning a `Maybe` which will
-- |either contain the resulting signal, or `Nothing` if the `Foldable`
-- |was empty.
mergeMany :: (Functor f, Foldable f) => f (Signal a) -> Maybe (Signal a)
mergeMany sigs = foldl mergeMaybe Nothing (Just <$> sigs)
  where mergeMaybe a Nothing = a
        mergeMaybe Nothing a = a
        mergeMaybe (Just a) (Just b) = Just (merge a b)

-- |Creates a past dependent signal. The function argument takes the value of
-- |the input signal, and the previous value of the output signal, to produce
-- |the new value of the output signal.
foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp = undefined

-- |Creates a signal which yields the current value of the second signal every
-- |time the first signal yields.
sampleOn :: Signal a -> Signal b -> Signal b
sampleOn = undefined

-- |Create a signal which only yields values which aren't equal to the previous
-- |value of the input signal.
dropRepeats :: (Eq a) => Signal a -> Signal a
dropRepeats = undefined

-- |Given a signal of effects with no return value, run each effect as it
-- |comes in.
runSignal :: Signal (IO ()) -> IO ()
runSignal sig = do
  sig `subscribe` \val -> val


-- |Takes a signal of effects of `a`, and produces an effect which returns a
-- |signal which will take each effect produced by the input signal, run it,
-- |and yield its returned value.
unwrap :: Signal (IO a) -> IO (Signal a)
unwrap = undefined

-- |Takes a signal and filters out yielded values for which the provided
-- |predicate function returns `false`.
filter :: (a -> Bool) -> a -> Signal a -> Signal a
filter = undefined

-- |Map a signal over a function which returns a `Maybe`, yielding only the
-- |values inside `Just`s, dropping the `Nothing`s.
filterMap :: (a -> Maybe b) -> b -> Signal a -> Signal b
filterMap f def sig = fromMaybe def <$> filter isJust (Just def) (f <$> sig)

-- |Turns a signal of arrays of items into a signal of each item inside
-- |each array, in order.
-- |
-- |Like `flatten`, but faster.
flattenArray :: Signal [a] -> a -> Signal a
flattenArray = undefined

-- |Turns a signal of collections of items into a signal of each item inside
-- |each collection, in order.
flatten :: (Functor f, Foldable f) => Signal (f a) -> a -> Signal a
flatten sig = flattenArray (sig ~> fold . fmap (: []) )

infixl 4 ~>
(~>) :: Signal a -> (a -> b) -> Signal b
sig ~> f = fmap f sig

instance Functor Signal where
  fmap fun sig = unsafePerformIO $ do
    let out = make $ fun $ get sig
    sig `subscribe` \val -> out `set` fun val
    return out

instance Applicative Signal where
  pure = constant
  fun <*> sig = unsafePerformIO $ do
    let f = get fun
    let out = make $ f (get sig)
    let produce = const $ out `set` f (get sig)
    fun `subscribe` produce
    sig `subscribe` produce
    return out

instance Semigroup (Signal a) where
  (<>) = merge
