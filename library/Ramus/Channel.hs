module Ramus.Channel where

import Ramus.Signal

newtype Channel a = Channel (Signal a)

-- |Creates a channel, which allows you to feed arbitrary values into a signal.
channel :: a -> IO (Channel a)
channel = return . Channel . make

-- |Sends a value to a given channel.
send :: Channel a -> a -> IO ()
send (Channel c) = set c

-- |Takes a channel and returns a signal of the values sent to it.
subscribe :: Channel a -> Signal a
subscribe (Channel c) = c
