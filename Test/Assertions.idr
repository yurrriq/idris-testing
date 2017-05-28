-- ---------------------------------------------------------- [ Assertions.idr ]
-- Module    : Assertions.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Testing with Assertions.
module Test.Assertions

import public Test.Utils

import Test.Generic

%access export

||| Assert that a given Boolean is `True`.
assertTrue : Bool -> IO Bool
assertTrue b = genericTest (Just "Assert True") b True (==)

||| Assert that a given Boolean is `False`.
assertFalse : Bool -> IO Bool
assertFalse b = genericTest (Just "Assert False") b False (==)

||| Assert that a `given` is equal to `expected`.
assertEquals : (Eq a, Show a) => (given : a) -> (expected : a) -> IO Bool
assertEquals g e = genericTest (Just "Assert Equals") g e (==)

||| Assert that `given` is not equal to `expected`.
assertNotEquals : (Eq a, Show a) => (given : a) -> (expected : a) -> IO Bool
assertNotEquals g e = genericTest (Just "Assert Not Equals") g e (\x,y => not (x == y))

||| Assert that a given `Maybe` is of the form `Just _`.
assertJust : Show a => Maybe a -> IO Bool
assertJust g = genericTest (Just "Assert Is Just") (isJust g) True (==)

||| Assert that a given `Maybe` is `Nothing`.
assertNothing : Show a => Maybe a -> IO Bool
assertNothing g = genericTest (Just "Assert Is Nothing") (isNothing g) True (==)

||| Assert that a given value is a `Left`-value.
assertLeft : (Show a, Show b) => Either a b -> IO Bool
assertLeft g = genericTest (Just "Assert is Left") (isLeft g) True (==)

||| Assert that a given value is a `Right`-value.
assertRight : (Show a, Show b) => Either a b -> IO Bool
assertRight g = genericTest (Just "Assert is Right") (isRight g) True (==)

-- --------------------------------------------------------------------- [ EOF ]
