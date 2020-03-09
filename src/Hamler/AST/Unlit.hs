-- | Part of this code is from "Report on the Programming Language Haskell",
--   version 1.2, appendix C.
{-# OPTIONS_GHC -Wwarn #-}
module Hamler.AST.Unlit (unlit) where

import Data.Char
import Data.List (isPrefixOf)

data Classified = Program String | Blank | Comment
                | Include Int String | Pre String

classify :: [String] -> [Classified]
classify []                = []
classify (('\\':x):xs) | x == "begin{code}" = Blank : allProg xs
   where allProg [] = []  -- Should give an error message,
                          -- but I have no good position information.
         allProg (('\\':x):xs) |  "end{code}"`isPrefixOf`x = Blank : classify xs
         allProg (x:xs) = Program x:allProg xs
classify (('>':x):xs)      = Program (' ':x) : classify xs
classify (('#':x):xs)      = (case words x of
                                (line:rest) | all isDigit line
                                   -> Include (read line) (unwords rest)
                                _  -> Pre x
                             ) : classify xs
--classify (x:xs) | "{-# LINE" `isPrefixOf` x = Program x: classify xs
classify (x:xs) | all isSpace x = Blank:classify xs
classify (x:xs)                 = Comment:classify xs

unclassify :: Classified -> String
unclassify (Program s) = s
unclassify (Pre s)     = '#':s
unclassify (Include i f) = '#':' ':show i ++ ' ':f
unclassify Blank       = ""
unclassify Comment     = ""

-- | 'unlit' takes a filename (for error reports), and transforms the
--   given string, to eliminate the literate comments from the program text.
unlit :: FilePath -> String -> String
unlit file lhs = (unlines
                 . map unclassify
                 . adjacent file (0::Int) Blank
                 . classify) (inlines lhs)

adjacent :: FilePath -> Int -> Classified -> [Classified] -> [Classified]
adjacent file 0 _             (x              :xs) = x : adjacent file 1 x xs -- force evaluation of line number
adjacent file n y@(Program _) (x@Comment      :xs) = error (message file n "program" "comment")
adjacent file n y@(Program _) (x@(Include i f):xs) = x: adjacent f    i     y xs
adjacent file n y@(Program _) (x@(Pre _)      :xs) = x: adjacent file (n+1) y xs
adjacent file n y@Comment     (x@(Program _)  :xs) = error (message file n "comment" "program")
adjacent file n y@Comment     (x@(Include i f):xs) = x: adjacent f    i     y xs
adjacent file n y@Comment     (x@(Pre _)      :xs) = x: adjacent file (n+1) y xs
adjacent file n y@Blank       (x@(Include i f):xs) = x: adjacent f    i     y xs
adjacent file n y@Blank       (x@(Pre _)      :xs) = x: adjacent file (n+1) y xs
adjacent file n _             (x@next         :xs) = x: adjacent file (n+1) x xs
adjacent file n _             []                   = []

message :: String -> Int -> String -> String -> String
message "\"\"" n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message []     n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message file   n p c = "In file " ++ file ++ " at line "++show n++": "++p++ " line before "++c++" line.\n"


-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
inlines :: String -> [String]
inlines s = lines' s id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc [] : lines' s id      -- DOS
  lines' ('\^M':s)      acc = acc [] : lines' s id      -- MacOS
  lines' ('\n':s)       acc = acc [] : lines' s id      -- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))
