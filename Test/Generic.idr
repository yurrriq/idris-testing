-- ------------------------------------------------------------- [ Generic.idr ]
-- Module    : Generic.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Generic Tests
module Test.Generic

import public Test.Utils

genericTest : Show a => (title : Maybe String)
                     -> (given : a)
                     -> (expected : a)
                     -> (tFunc : a -> a -> Bool)
                     -> IO ()
genericTest title g e eq = do
  putStrLn $ unwords ["Test:" , fromMaybe "Unnamed Test" title]
  if eq g e
    then pure ()
    else with List do
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

-- --------------------------------------------------------------------- [ EOF ]
