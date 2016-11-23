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
assertTrue : Bool -> IO ()
assertTrue b = genericTest (Just "Assert True") b True (==)

||| Assert that a given Boolean is `False`.
assertFalse : Bool -> IO ()
assertFalse b = genericTest (Just "Assert False") b False (==)

||| Assert that a `given` is equal to `expected`.
assertEqual : (Eq a, Show a) => (given : a) -> (expected : a) -> IO ()
assertEqual g e = genericTest (Just "Assert Equal") g e (==)

||| Assert that `given` is not equal to `expected`.
assertNotEqual : (Eq a, Show a) => (given : a) -> (expected : a) -> IO ()
assertNotEqual g e = genericTest (Just "Assert Not Equal") g e (/=)

||| Assert that a given `Maybe` is of the form `Just _`.
assertJust : Show a => Maybe a -> IO ()
assertJust g = genericTest (Just "Assert Is Just") (isJust g) True (==)

||| Assert that a given `Maybe` is `Nothing`.
assertNothing : Show a => Maybe a -> IO ()
assertNothing g = genericTest (Just "Assert Is Nothing") (isNothing g) True (==)

||| Assert that a given value is a `Left`-value.
assertLeft : (Show a, Show b) => Either a b -> IO ()
assertLeft g = genericTest (Just "Assert is Left") (isLeft g) True (==)

||| Assert that a given value is a `Right`-value.
assertRight : (Show a, Show b) => Either a b -> IO ()
assertRight g = genericTest (Just "Assert is Right") (isRight g) True (==)

-- --------------------------------------------------------------------- [ EOF ]
