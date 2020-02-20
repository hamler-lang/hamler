module Hamler
    ( hello
    , parseStr
    , parseFile
    , whiteSpace
    ) where

import Hamler.Parser.Lexer (whiteSpace)
import Hamler.Parser (parseStr, parseFile)

hello :: IO ()
hello = putStrLn "Hello, Hamler!"
