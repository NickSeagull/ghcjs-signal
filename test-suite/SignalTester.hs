module SignalTester
    ( shouldYield
    )
where

import Signal
import Data.IORef

shouldYield :: (Eq a, Show a)
              => Signal a
              -> [a]
              -> IO ()
shouldYield sig vals = do
    remaining <- newIORef vals
    let getNext val = do
            nextValues <- readIORef remaining
            case nextValues of
                (x : xs) ->
                    if x /= val
                        then error $ "Expected " ++ show x ++ " but got " ++ show val
                        else case xs of
                            [] -> return ()
                            _ -> writeIORef remaining xs
                [] -> error "Unexpected emptiness"
    runSignal $ sig ~> getNext