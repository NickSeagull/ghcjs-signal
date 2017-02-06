{-# LANGUAGE TypeOperators #-}

import Prelude hiding (filter)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.IO ()
import Data.Semigroup
import Data.Maybe
import Ramus.Signal
import Ramus.Channel as Channel
import Ramus.Time
import SignalTester

type A = Int
type B = Int
type C = Int
type (~>) a b = Fun a b

main :: IO ()
main = hspec $ parallel $ do

    describe "The Signal tester" $ 
        it "can check if a Signal contains the values or not" $
            constant "Foo" `shouldYield` ["Foo"] 

    describe "A Signal" $ do

        it "can contain an IO action, and is able to run it after" $
            runSignal $ constant (return ())

        it "is a functor, it satisfies the identity law" $ 
            property functorIdentity

        it "is a functor, it satisfies the composition law" $ 
            property functorComposition

        it "is an applicative, it satisifies the identity law" $ 
            property applicativeIdentity

        it "is an applicative, it satisifies the homomorphism law" $ 
            property applicativeHomomorphism

        it "is an applicative, it satisifies the composition law" $ 
            property applicativeComposition

        it "is an applicative, it satisifies the interchange law" $ 
            property applicativeInterchange

        it "is able to merge with another signal, yielding in order" $
            property semigroupMerge
    
        it "is able to merge with multiple signals, yielding in order" $
            property semigroupMergeMany

        it "is able to map a function over each value that will be yielded" $
            property mapFunctionsProperty

        it "is able to drop repeated values in a sequence" $
            property dropRepeatsProperty
        
        it "can reduce values with foldp" $
            foldp (+) 0 (tick 1 1 [1, 2, 3, 4, 5])
            `shouldYield` [1, 3, 6, 10, 15]

        it "is able to filter out values with filter" $
            filter (< 5) 0 (tick 1 1 [5, 3, 8, 4])
            `shouldYield` [0, 3, 4]

        it "is able to filter Maybe values with filterMap" $
            filterMap (\n -> if n < 5 then Just n else Nothing)
                 0 (tick 1 1 [5, 3, 8, 4]) 
                 `shouldYield` [0, 3, 4]

        {- Leaves the first value off always}
        it "is able to flatten the values" $
            flatten (tick 1 1 [[1, 2], [3, 4], [], [5, 6, 7]]) 0
            `shouldYield` [1, 2, 3, 4, 5, 6, 7]
        -}

        it "is able to sum values with foldp" $
            foldp (+) 0 (tick 1 1 [1, 2, 3, 4, 5])
            `shouldYield` [1, 3, 6, 10, 15]

        it "can be delayed, but yields the same results" $
            delay 40.0 (tick 1 1 [1, 2, 3, 4, 5])
            `shouldYield` [1, 2, 3, 4, 5]

        it "yields true only once for multiple yields with since" $
            since 10.0 (tick 1 1 [1, 2, 3])
            `shouldYield` [False, True, False]


    describe "A Channel" $ do

        it "'s subscriptions yield when we send to it" $ do
            chan <- Channel.channel 1
            runSignal $ tick 1 1 [2, 3, 4] ~> Channel.send chan
            Channel.subscribe chan `shouldYield` [2, 3, 4]


functorIdentity :: A
                -> IO ()
functorIdentity x = 
    (id <$> constant x)
    `shouldYield` [x]


functorComposition :: A ~> B
                   -> B ~> C
                   -> A
                   -> IO ()
functorComposition _F _G x = 
    (f <$> g <$> constant x)
    `shouldYield` [f (g x)]
  where 
    f = apply _F
    g = apply _G


applicativeIdentity :: A
                    -> IO ()
applicativeIdentity x =
    (pure id <*> pure x)
    `shouldYield` [x]


applicativeHomomorphism :: A ~> B
                        -> A
                        -> IO ()
applicativeHomomorphism _F x =
    (pure f <*> pure x)
    `shouldYield` [f x]
  where f = apply _F


applicativeComposition :: B ~> C
                       -> A ~> B
                       -> A
                       -> IO ()
applicativeComposition _F _G x =
    (pure (.) <*> apf <*> apg <*> apx)
    `shouldYield` [(f . g) x]
  where
    f = apply _F
    g = apply _G
    apf = pure f
    apg = pure g
    apx = pure x


applicativeInterchange :: A
                       -> A ~> B
                       -> IO ()
applicativeInterchange y _U = 
    (pure ($ y) <*> apu)
    `shouldYield` [u y]
  where
    u = apply _U
    apu = pure u


semigroupMerge :: A
               -> A
               -> IO ()
semigroupMerge x y =
    (constant x <> constant y)
    `shouldYield` [x]


semigroupMergeMany :: A
                   -> [A]
                   -> IO ()
semigroupMergeMany x xs =
    fromMaybe (constant 1337) (mergeMany testSignals)
    `shouldYield` [x]
  where
    testSignals = constant <$> (x:xs)


mapFunctionsProperty :: [A]
                     -> A ~> B
                     -> Property
mapFunctionsProperty lst _F =
    length lst > 1 ==> 
    (f <$> tick 1 1 lst ) `shouldYield` (f <$> lst)
  where
    f = apply _F


dropRepeatsProperty :: [A]
                    -> Property
dropRepeatsProperty lst =
    length lst > 1 ==>
    dropRepeats (tick 1 1 duplicated) `shouldYield` lst
  where
    duplicated = concatMap (\ x -> [x, x]) lst