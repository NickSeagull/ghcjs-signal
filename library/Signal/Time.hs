module Signal.Time where

import Prelude hiding (filter)
import Signal

type Time = Float

millisecond :: Time
millisecond = 1.0

second :: Time
second = 1000.0

-- |Creates a signal which yields the current time (according to `now`) every
-- |given number of milliseconds.
every :: Time -> Signal Time
every = undefined

-- |Returns the number of milliseconds since an arbitrary, but constant, time
-- |in the past.
now :: IO Time
now = undefined

-- |Takes a signal and delays its yielded values by a given number of
-- |milliseconds.
delay :: Time -> Signal a -> Signal a
delay = undefined

-- |Takes a signal and a time value, and creates a signal which yields `True`
-- |when the input signal yields, then goes back to `False` after the given
-- |number of milliseconds have elapsed, unless the input signal yields again
-- |in the interim.
since :: Time -> Signal a -> Signal Bool
since = undefined

-- |Takes a signal and a time value, and creates a signal which waits to yield
-- |the next result until the specified amount of time has elapsed. It then
-- |yields only the newest value from that period. New events during the debounce
-- |period reset the delay.
debounce :: Time -> Signal a -> Signal a
debounce t s =
  let leading = whenChangeTo False $ since t s
  in sampleOn leading s
  where
    whenEqual value = filter (value ==) value
    whenChangeTo value input = whenEqual value $ dropRepeats input
