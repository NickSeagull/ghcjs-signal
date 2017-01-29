module Signal.Channel where

import Signal

newtype Channel a = Channel a

-- |Creates a channel, which allows you to feed arbitrary values into a signal.
channel :: a -> IO (Channel a)
channel = undefined

-- |Sends a value to a given channel.
send :: Channel a -> a -> IO ()
send = undefined

-- |Takes a channel and returns a signal of the values sent to it.
subscribe :: Channel a -> Signal a
subscribe = undefined
