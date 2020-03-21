-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.CodeGen.CoreErl.Parser
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alex Kropivny <alex.kropivny@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- CoreErlang Parser.
-- <http://www.it.uu.se/research/group/hipe/cerl/>
--
-----------------------------------------------------------------------------

module Hamler.CodeGen.CoreErl.Parser (
    parseFile,
    parseModule,
    ParseError
  ) where

import Hamler.CodeGen.CoreErl.Syntax

import Control.Monad ( liftM )
import Data.Char ( isControl, chr )
import Numeric ( readOct )

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Token
        ( makeTokenParser, TokenParser )
import Text.ParserCombinators.Parsec.Language

import System.IO

-- Lexical definitions

uppercase :: Parser Char
uppercase = upper

lowercase :: Parser Char
lowercase = lower

inputchar :: Parser Char
inputchar = noneOf "\n\r"

control :: Parser Char
control = satisfy isControl

namechar :: Parser Char
namechar = uppercase <|> lowercase <|> digit <|> oneOf "@_"

escape :: Parser Char
escape = do char '\\'
            s <- octal <|> ctrl <|> escapechar
            return s

octal :: Parser Char
octal = do chars <- tryOctal
           let [(o, _)] = readOct chars
           return (chr o)

tryOctal :: Parser [Char]
tryOctal = choice [ try (count 3 octaldigit),
                    try (count 2 octaldigit),
                    try (count 1 octaldigit) ]

octaldigit :: Parser Char
octaldigit = oneOf "01234567"

ctrl :: Parser Char
ctrl = char '^' >> ctrlchar

ctrlchar :: Parser Char
ctrlchar = satisfy (`elem` ['\x0040'..'\x005f'])

escapechar = oneOf "bdefnrstv\"\'\\"

-- Terminals

integer :: Parser Integer
integer = do i <- positive <|> negative <|> decimal
             whiteSpace -- TODO: buff
             return $ i

positive :: Parser Integer
positive = do char '+'
              p <- decimal
              return p

negative :: Parser Integer
negative = do char '-'
              n <- decimal
              return $ negate n

-- float :: Parser Double
-- float = sign?digit+.digit+((E|e)sign?digit+)?

atom :: Parser EAtom
atom = do char '\''
--          ((inputchar except control and \ and ')|escape)*
--          inputchar = noneOf "\n\r"
          a <- many (noneOf "\n\r\\\'")
          char '\''
          whiteSpace -- TODO: buff
          return $ EAtom a

echar :: Parser ELiteral
-- char = $((inputchar except control and space and \)|escape)
echar = do char '$'
           c <- noneOf "\n\r\\ "
           whiteSpace -- TODO: buff
           return $ ELChar c

estring :: Parser ELiteral
-- string = "((inputchar except control and \\ and \"")|escape)*"
estring = do char '"'
             s <- many $ noneOf "\n\r\\\""
             char '"'
             return $ ELString s

variable :: Parser EVar
-- variable = (uppercase | (_ namechar)) namechar*
variable = identifier

-- Non-terminals

emodule :: Parser (EAnn EModule)
emodule = annotated amodule

amodule :: Parser EModule
amodule = do reserved "module"
             name <- atom
             funs <- exports
             attrs <- attributes
             fundefs <- many fundef
             reserved "end"
             return $ EModule name funs attrs fundefs

exports :: Parser [EFunction]
exports = brackets $ commaSep function

attributes :: Parser [(EAtom,EConst)]
attributes = do reserved "attributes"
                brackets (commaSep $ do a <- atom
                                        symbol "="
                                        c <- constant
                                        return (a,c))

constant :: Parser EConst
constant = liftM ECLit (try literal) <|>
           liftM ECTuple (tuple constant) <|>
           liftM ECList (elist constant)

fundef :: Parser EFunDef
fundef = do name <- annotated function
            symbol "="
            body <- annotated lambda
            return $ EFunDef name body

function :: Parser EFunction
function = do a <- atom
              char '/'
              i <- decimal
              whiteSpace -- TODO: buff
              return $ EFunction (a,i)

literal :: Parser ELiteral
literal = try (liftM ELFloat float) <|> liftM ELInt integer <|>
          liftM ELAtom atom <|> nil <|> echar <|> estring

nil :: Parser ELiteral
nil = brackets (return ELNil)

expression :: Parser EExps
expression =  try (liftM EExps (annotated $ angles $ commaSep (annotated sexpression))) <|>
              liftM EExp (annotated sexpression)

sexpression :: Parser EExp
sexpression = app <|> ecatch <|> ecase <|> elet <|>
              liftM EFun (try function) {- because of atom -} <|>
              lambda <|> letrec <|> liftM EBinary (ebinary expression) <|>
              liftM EList (try $ elist expression) {- because of nil -} <|>
              liftM ELit literal <|> modcall <|> op <|> receive <|>
              eseq <|> etry <|> liftM ETuple (tuple expression) <|>
              liftM EVar variable

app :: Parser EExp
app = do reserved "apply"
         e1 <- expression
         eN <- parens $ commaSep expression
         return $ EApp e1 eN

ecatch :: Parser EExp
ecatch = do reserved "catch"
            e <- expression
            return $ ECatch e

ebinary :: Parser a -> Parser [EBitString a]
ebinary p = do symbol "#"
               bs <- braces (commaSep (bitstring p))
               symbol "#"
               return bs

bitstring :: Parser a -> Parser (EBitString a)
bitstring p = do symbol "#"
                 e0 <- angles p
                 es <- parens (commaSep expression)
                 return $ EBitString e0 es

ecase :: Parser EExp
ecase = do reserved "case"
           exp <- expression
           reserved "of"
           alts <- many1 (annotated clause)
           reserved "end"
           return $ ECase exp alts

clause :: Parser EAlt
clause = do pat <- patterns
            g <- guard
            symbol "->"
            exp <- expression
            return $ EAlt pat g exp

patterns :: Parser EPats
patterns = liftM EPat pattern <|>
           liftM EPats (angles $ commaSep pattern)

pattern :: Parser EPat
pattern = liftM EPAlias (try alias) {- because of variable -} <|> liftM EPVar variable <|>
          liftM EPLit (try literal) {- because of nil -} <|> liftM EPTuple (tuple pattern) <|>
          liftM EPList (elist pattern) <|> liftM EPBinary (ebinary pattern)

alias :: Parser EAlias
alias = do v <- variable
           symbol "="
           p <- pattern
           return $ EAlias v p

guard :: Parser EGuard
guard = do reserved "when"
           e <- expression
           return $ EGuard e

elet :: Parser EExp
elet = do reserved "let"
          vars <- variables
          symbol "="
          e1 <- expression
          symbol "in"
          e2 <- expression
          return $ ELet (vars,e1) e2

variables :: Parser [EVar]
variables = do { v <- variable; return [v]} <|> (angles $ commaSep variable)

lambda :: Parser EExp
lambda = do reserved "fun"
            vars <- parens $ commaSep variable
            symbol "->"
            expr <- expression
            return $ ELambda vars expr

letrec :: Parser EExp
letrec = do reserved "letrec"
            defs <- many fundef
            reserved "in"
            e <- expression
            return $ ELetRec defs e

elist :: Parser a -> Parser (EList a)
elist a = brackets $ list a

list :: Parser a -> Parser (EList a)
list elem = do elems <- commaSep1 elem
               option (EL elems) (do symbol "|"
                                     t <- elem
                                     return $ ELL elems t)

modcall :: Parser EExp
modcall = do reserved "call"
             e1 <- expression
             symbol ":"
             e2 <- expression
             eN <- parens $ commaSep expression
             return $ EModCall (e1, e2) eN

op :: Parser EExp
op = do reserved "primop"
        a <- atom
        e <- parens $ commaSep expression
        return $ EOp a e

receive :: Parser EExp
receive = do reserved "receive"
             alts <- many $ annotated clause
             to <- timeout
             return $ ERec alts to

timeout :: Parser ETimeOut
timeout = do reserved "after"
             e1 <- expression
             symbol "->"
             e2 <- expression
             return $ ETimeOut e1 e2

eseq :: Parser EExp
eseq =  do reserved "do"
           e1 <- expression
           e2 <- expression
           return $ ESeq e1 e2

etry :: Parser EExp
etry = do reserved "try"
          e1 <- expression
          reserved "of"
          v1 <- variables
          symbol "->"
          e2 <- expression
          reserved "catch"
          v2 <- variables
          symbol "->"
          e3 <- expression
          return $ ETry e1 (v1,e1) (v2,e2)

tuple :: Parser a -> Parser [a]
tuple elem = braces $ commaSep elem

annotation :: Parser [EConst]
annotation = do symbol "-|"
                cs <- brackets $ many constant
                return $ cs

annotated :: Parser a -> Parser (EAnn a)
annotated p = parens (do e <- p
                         cs <- annotation
                         return $ EAnn e cs)
              <|>
              do e <- p
                 return $ EConstr e

lexer :: TokenParser ()
lexer = makeTokenParser
            (emptyDef {
             --    commentStart = "",
             --    commentEnd = "",
                   commentLine = "%",
             --    nestedComments = True,
                   identStart = upper <|> char '_',
                   identLetter = namechar
             --    opStart,
             --    opLetter,
             --    reservedNames,
             --    reservedOpNames,
             --    caseSensitive = True,
               })

angles = Token.angles lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
commaSep = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer
decimal = Token.decimal lexer
float = Token.float lexer
identifier = Token.identifier lexer
natural = Token.natural lexer
parens = Token.parens lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer

runLex :: Show a => Parser a -> String -> IO ()
runLex p file = do input <- readFile file
                   parseTest (do whiteSpace
                                 x <- p
                                 eof
                                 return x) input
                   return ()

-- | Parse of a string, which should contain a complete CoreErlang module
parseModule :: String -> Either ParseError (EAnn EModule)
parseModule input = parse (do whiteSpace
                              x <- emodule
                              eof
                              return x) "" input


parseFile :: FilePath -> IO (Either ParseError (EAnn EModule))
parseFile fp = do
  h <- openFile fp ReadMode
  content <- hGetContents h
  return $ parseModule content

