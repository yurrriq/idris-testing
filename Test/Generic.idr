-- ------------------------------------------------------------- [ Generic.idr ]
-- Module    : Generic.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Generic Testing.
module Test.Generic

import public Test.Utils

%access export

||| Run a generic test.
|||
||| @ title     an optional test title
||| @ given     the string to parse
||| @ expected  the expected result
||| @ tFunc     the testing function to compare the results
genericTest : Show a => (title : Maybe String)
                     -> (given : a)
                     -> (expected : a)
                     -> (tFunc : a -> a -> Bool)
                     -> IO Bool
genericTest title g e eq = do
  putStrLn $ unwords ["Test:" , fromMaybe "Unnamed Test" title]
  if eq g e
    then pure True
    else do
       putStrLn $ unwords [
             errLine
           , "\n"
           , "Error:\n\n"
           , "Given:\n\t"
           , show g
           , "\n"
           , "Expected:\n\t"
           , show e
           , "\n"
           , errLine
           , "\n"
           ]
       pure False

-- --------------------------------------------------------------------- [ EOF ]
