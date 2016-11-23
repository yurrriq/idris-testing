-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Common Testing Utilities.
module Test.Utils

import Data.Vect

%access export

||| Construct a string of `n` repetitions of `c`.
|||
||| @ n the number of times to repeat it
||| @ c the character to repeat
fancyLine : (n : Nat) -> (c : Char) -> String
fancyLine n c = pack $ List.replicate n c

||| A string comprised of 40 `-` characters.
infoLine : String
infoLine = fancyLine 40 '-'

||| A string comprised of 40 `=` characters.
succLine : String
succLine = fancyLine 40 '='

||| A string comprised of 40 `+` characters.
errLine : String
errLine = fancyLine 40 '+'

||| Return a string with a given string between two `infoLine`s.
heading : String -> String
heading s = unlines [infoLine, s, infoLine]

-- Lifted/modified from String.Extra (https://github.com/yurrriq/idris-string)
private
pluralize : String -> String -> Nat -> String
pluralize singular plural count = unwords $
  if 1 == count
     then ["1", singular]
     else [show count, plural]

||| Return a summary of the given test `results`.
|||
||| ```idris example
||| summary [True, False, True]
||| ```
|||
||| @ results a vector of Boolean test results
summary : (results : Vect n Bool) -> String
summary {n} results =
  pluralize "test" "tests" n ++ ", " ++
  pluralize "failure" "failures" (fst (filter not results))

||| Run a list of tests and print a summary.
|||
||| @ tests a list of test to run, i.e. I/O actions returning a Boolean
runTests : (tests : List (IO Bool)) -> IO ()
runTests tests = putStrLn !(summary <$> sequence (fromList tests))

-- --------------------------------------------------------------------- [ EOF ]
