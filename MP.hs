module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-- lookUp takes a search string and a list of string/item pairs as arguments 
-- and outputs the list of items for which the specific string matches the
-- search string 
lookUp :: String -> [(String, a)] -> [a]
lookUp  str list
  = [x | (str', x) <- list, str == str']  

-- split operates recursively to break up a string given a list of separators 
-- within the string and returns a tuple with the separators and a list of the
-- words in the string (with "" in the place of separators within the list of
-- words to show where they go)
split :: [Char] -> String -> (String, [String])
split  _ ""
  = ("", [""])
split seps (y : ys)
  | elem y seps = (y : seps', "" : (x : xs)) 
  | otherwise   = (seps', (y : x) : xs)
    where
      (seps',(x : xs)) = split seps ys

-- combine performs the opposing action to split in that, when given a string of
-- separators and a list of strings it concatenates them into a single list of
-- strings
combine :: String -> [String] -> [String]
combine "" string 
  = string 
combine (sep : seps) (x : xs)
  = x : [sep] : combine seps xs

-- the function getKeywordDefs splits on the spaces in the strings in the
-- information file and returns a list of keyword/definition pairs by
-- concatenating and combining the variables from pattern matching the split
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs []
  = []
getKeywordDefs (x : xs)
  = (keyw, concat (combine spaces def)) : getKeywordDefs xs
    where 
      (_ : spaces, keyw : def ) = split " " x 
-- expand brings all of the functions together in the macroprocessor and takes
-- the template file and the info file and replaces the required words.
-- the main function allows this function to read and write the necessary files.
-- this function has also been tested in the command line as mentioned in the
-- specification.
expand :: FileContents -> FileContents -> FileContents
expand temp inf
  = concat (combine seps [replaceWord x keywds | x <- words])
    where
     (_,  defs)    = split "\n" inf
     keywds        = getKeywordDefs defs
     (seps, words) = split separators temp

-- Below is a helper function implemented in order to aid the definition of the
-- expand function
-- Here I have decided to return the original word if the keyword replacement
-- can not be found. This is done using replaceword'
replaceWord :: String -> KeywordDefs -> String
replaceWord "" _ 
  = ""
replaceWord word@(x : xs) keywd
  | x == '$'  = replaceword' (lookUp word keywd)
  | otherwise = word 
    where
      replaceword' :: [String] -> String
      replaceword' []       = word
      replaceword' (y : ys) = y

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

