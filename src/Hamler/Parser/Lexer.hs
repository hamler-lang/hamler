{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hamler.Lexer
-- Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
-- License     :  Apache Version 2.0
--
-- Maintainer  :  Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implement a lexical analyzer for hamler language.
--------------------------------------------------------------------------------

module Hamler.Parser.Lexer
  ( Parser
  , lexeme
  , whiteSpace
  , lineComment
  , blockComment
  , integer
  , float
  , string
  , parens
  , brackets
  , braces
  , semicolon
  , comma
  , colon
  , dot
  ) where

import Data.Char ( chr
                 , digitToInt
                 , isAsciiLower
                 , isAsciiUpper
                 , ord
                 )
import Data.Void (Void)
import Text.Megaparsec ( Parsec
                       , between
                       , chunk
                       , chunkToTokens
                       , manyTill
                       , notFollowedBy
                       , oneOf
                       , option
                       , optional
                       , satisfy
                       , sepBy
                       , skipSome
                       , takeP
                       , takeWhile1P
                       , try
                       )
import Text.Megaparsec.Char ( char
                            , digitChar
                            , space1
                            )

import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

type Parser = Parsec Void T.Text

lexeme :: forall a. Parser a -> Parser a
lexeme = Lexer.lexeme whiteSpace

whiteSpace :: Parser ()
whiteSpace = Lexer.space space1 lineComment blockComment

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "--"

blockComment :: Parser ()
blockComment = Lexer.skipBlockCommentNested "{-" "-}"

symbol :: T.Text -> Parser T.Text
symbol = Lexer.symbol whiteSpace

semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

integer :: Integral a => Parser a
integer = Lexer.signed (pure ()) $ lexeme Lexer.decimal

float :: Parser Double
float = Lexer.signed (pure ()) $ lexeme Lexer.float

string :: Parser String
string = char '"' >> manyTill Lexer.charLiteral (char '"')

-- | Parser for an expression between "(" and ")".
parens :: forall a. Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parser for an expression between "[" and "]".
brackets :: forall a. Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parser for an expression between "{" and "}".
braces :: forall a. Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

