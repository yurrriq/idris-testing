-- ------------------------------------------------------------- [ Parsing.idr ]
-- Module    : Parsing.idr
-- Copyright : (c) Jan de Muijnck-Hughes
-- License   : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
||| Lightyear-based Parser Testing.
module Test.Parsing

import public Lightyear
import public Lightyear.Strings

import public Test.Utils

%access export

||| Run a parse test that is expected to pass.
|||
||| @ title an optional test title
||| @ p     the parser to test
||| @ inStr the string to parse
||| @ exp   the expected result of parsing `inStr` using `p`
||| @ tFunc the testing function to compare the results
parseTestG : Show a => (title : Maybe String)
                   -> (p : Parser a)
                   -> (inStr : String)
                   -> (exp : a)
                   -> (tFunc : a -> a -> Bool)
                   -> IO Bool
parseTestG title p inStr exp eq = do
  putStrLn $ unwords ["Test:", fromMaybe "Unnamed Test" title]
  case parse p inStr of
    Left err  => do
         putStrLn $ unlines [
               errLine
             , "Unexpected Parse Error:\n"
             , err
             , errLine
             ]
         pure False
    Right res => do
      if eq res exp
        then pure True
        else do
           putStrLn $ unlines [
                 errLine
               , "Error:\n"
               , "Given:"
               , "\t" ++ show inStr
               , "Made"
               , "\t" ++ show res
               , "Expected:"
               , "\t" ++ show exp
               , errLine
               ]
           pure False

||| Run a parse test that is expected to fail.
|||
||| @ title an optional test title
||| @ p     the parser to test
||| @ inStr the string to parse
parseTestB : Show a => (title : Maybe String)
                   -> (p : Parser a)
                   -> (inStr : String)
                   -> IO Bool
parseTestB title p inStr = do
  putStrLn $ unwords ["Begin Test:", fromMaybe "Unnamed Test" title]
  case parse p inStr of
    Left err  => pure True
    Right res => do
        putStrLn $ unlines [
                 errLine
               , "Error:\n"
               , "Given:"
               , "\t" ++ show inStr
               , "Was expected to fail"
               , errLine
               ]
        pure False

||| Run a parse test to ensure a parse can parse a string.
|||
||| @ title an optional test title
||| @ p     the parser to test
||| @ inStr the string to parse
canParse : Show a => (title : Maybe String)
                   -> (p : Parser a)
                   -> (inStr : String)
                   -> IO Bool
canParse title p inStr = do
  putStrLn $ unwords ["Begin Test:", fromMaybe "Unnamed Test" title]
  case parse p inStr of
    Left err  => do
         putStrLn $ unlines [
               errLine
             , "Unexpected Parse Error:\n"
             , err
             , errLine
             ]
         pure False
    Right res => pure True

||| Run a parse test to ensure a parse cannot parse a string.
|||
||| @ title an optional test title
||| @ p     the parser to test
||| @ inStr the string to parse
canParseNot : Show a => (title : Maybe String)
                     -> (p : Parser a)
                     -> (inStr : String)
                     -> IO Bool
canParseNot title p inStr = do
  putStrLn $ unwords ["Begin Test:", fromMaybe "Unnamed Test" title]
  case parse p inStr of
    Left err  => pure True
    Right res => with List do
         putStrLn $ unlines [
               errLine
             , "Unexpected Parse Success:\n"
             , show res
             , errLine
             ]
         pure False

-- --------------------------------------------------------------------- [ EOF ]
