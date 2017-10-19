module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
lookUp  str list
  = [x | (str', x) <- list, str == str']  

split :: [Char] -> String -> (String, [String])
split  _ ""
  = ("", [""])
split seps (y : ys)
  | elem y seps = (y : seps', "" : (x : xs)) 
  | otherwise   = (seps', (y : x) : xs)
    where
      (seps',(x : xs)) = split seps ys

combine :: String -> [String] -> [String]
combine = error "TODO: implement combine"

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs = error "TODO: implement getKeywordDefs"

expand :: FileContents -> FileContents -> FileContents
expand = error "TODO: implement expand"

-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String



main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")

