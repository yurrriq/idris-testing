-- --------------------------------------------------------------- [ Utils.idr ]
-- Module    : Utils.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Common Testing Utilities.
module Test.Utils

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

||| Run a list of tests.
|||
||| @ tests a list of test to run, i.e. I/O actions returning a Boolean
runTests : List (IO ()) -> IO ()
runTests Nil     = do putStrLn "All Tests have passed"; putStrLn succLine
runTests (t::ts) = do t; runTests ts

-- --------------------------------------------------------------------- [ EOF ]
