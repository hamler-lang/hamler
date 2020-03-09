{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.Lexer
-- Copyright   :  (c) The GHC Team, 1997-2000
--                (c) Niklas Broberg, 2004-2009
--                (c) Feng Lee, 2020
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
--                Feng Lee <feng@emqx.io>
-- Stability   :  stable
-- Portability :  portable
--
-- Lexer for the Hamler Languange.
--
-----------------------------------------------------------------------------

-- ToDo: Introduce different tokens for decimal, octal and hexadecimal (?)
-- ToDo: FloatTok should have three parts (integer part, fraction, exponent) (?)
-- ToDo: Use a lexical analyser generator (lx?)

module Hamler.AST.Lexer (
    Token(..), Loc(..),
    lexTokenStream, lexTokenStreamWithMode,
    showToken, lexer, topLexer
    ) where

import Hamler.AST.ParseMonad
import Hamler.AST.SrcLoc hiding (loc)
import Hamler.AST.Comments
import Hamler.AST.Extension
import Hamler.AST.ExtScheme

import Prelude hiding (id, exponent)
import Data.Char
import Data.Ratio
import Data.List (intercalate, isPrefixOf)
import Control.Monad (when)

-- import Debug.Trace (trace)

data Token
        = VarId String
        | LabelVarId String
        | QVarId (String,String)
        | IDupVarId (String)        -- duplicable implicit parameter
        | ILinVarId (String)        -- linear implicit parameter
        | ConId String
        | QConId (String,String)
        | DVarId [String]       -- to enable varid's with '-' in them
        | VarSym String
        | ConSym String
        | QVarSym (String,String)
        | QConSym (String,String)
        | IntTok (Integer, String)
        | FloatTok (Rational, String)
        | Character (Char, String)
        | StringTok (String, String)
        | IntTokHash (Integer, String)        -- 1#
        | WordTokHash (Integer, String)       -- 1##
        | FloatTokHash (Rational, String)     -- 1.0#
        | DoubleTokHash (Rational, String)    -- 1.0##
        | CharacterHash (Char, String)        -- c#
        | StringHash (String, String)         -- "Hello world!"#

-- Symbols

        | LeftParen
        | RightParen
        | LeftHashParen
        | RightHashParen
        | SemiColon
        | LeftCurly
        | RightCurly
        | VRightCurly           -- a virtual close brace
        | LeftSquare
        | RightSquare
        | Comma
        | Underscore
        | BackQuote

-- Reserved operators

        | Dot           -- reserved for use with 'forall x . x'
        | DotDot
        | Colon
        | QuoteColon
        | DoubleColon
        | Equals
        | Backslash
        | Bar
        | LeftArrow
        | RightArrow
        | At
        | TApp -- '@' but have to check for preceeding whitespace
        | Tilde
        | DoubleArrow
        | Minus
        | Exclamation
        | Star

-- Pragmas

        | PragmaEnd                     -- #-}
        | RULES
        | INLINE Bool
        | INLINE_CONLIKE
        | SPECIALISE
        | SPECIALISE_INLINE Bool
        | SOURCE
        | DEPRECATED
        | WARNING
        | SCC
        | GENERATED
        | CORE
        | UNPACK
        | NOUNPACK
        | OPTIONS (Maybe String,String)
        | LANGUAGE
        | ANN
        | MINIMAL
        | NO_OVERLAP
        | OVERLAP
        | OVERLAPPING
        | OVERLAPPABLE
        | OVERLAPS
        | INCOHERENT
        | COMPLETE

-- Reserved Ids

        | KW_As
        | KW_By         -- transform list comprehensions
        | KW_Case
        | KW_Class
        | KW_Data
        | KW_Default
        | KW_Deriving
        | KW_Do
        | KW_MDo
        | KW_Else
        | KW_Family     -- indexed type families
        | KW_Forall     -- universal/existential types
        | KW_Fun
        | KW_Hiding
        | KW_If
        | KW_Import
        | KW_In
        | KW_Infix
        | KW_InfixL
        | KW_InfixR
        | KW_Instance
        | KW_Let
        | KW_Module
        | KW_NewType
        | KW_Of
        | KW_Then
        | KW_Type
        | KW_Where
        | KW_Qualified
        | KW_Pattern
        | KW_Stock
        | KW_Anyclass
        | KW_Via

                -- FFI
        | KW_Foreign
        | KW_Export
        | KW_StdCall
        | KW_CCall
        | KW_CPlusPlus
        | KW_DotNet
        | KW_Jvm
        | KW_Js
        | KW_JavaScript
        | KW_CApi

        | EOF
        deriving (Eq,Show)

-- | Lex a string into a list of Haskell 2010 source tokens.
lexTokenStream :: String -> ParseResult [Loc Token]
lexTokenStream = lexTokenStreamWithMode defaultParseMode

-- | Lex a string into a list of Haskell source tokens, using an explicit mode.
lexTokenStreamWithMode :: ParseMode -> String -> ParseResult [Loc Token]
lexTokenStreamWithMode mode = runParserWithMode mode lexIt
  where lexIt :: P [Loc Token]
        lexIt = runL go return
        go :: Lex [Loc Token] [Loc Token]
        go = do
          ltok <- topLexer
          case ltok of
            Loc _ EOF -> return []
            _ -> do ts <- go
                    return (ltok:ts)


reserved_ops :: [(String,(Token, Maybe ExtScheme))]
reserved_ops = [
 ( "..", (DotDot,       Nothing) ),
 ( ":",  (Colon,        Nothing) ),
 ( "::", (DoubleColon,  Nothing) ),
 ( "=",  (Equals,       Nothing) ),
 ( "\\", (Backslash,    Nothing) ),
 ( "|",  (Bar,          Nothing) ),
 ( "<-", (LeftArrow,    Nothing) ),
 ( "->", (RightArrow,   Nothing) ),
 ( "@",  (At,           Nothing) ),
 ( "~",  (Tilde,        Nothing) ),
 ( "=>", (DoubleArrow,  Nothing) ),
 ( "*",  (Star,         Just (Any [KindSignatures])) ),
 -- Unicode notation
 ( "\x2190",    (LeftArrow,     Just (Any  [UnicodeSyntax])) ),
 ( "\x2192",    (RightArrow,    Just (Any  [UnicodeSyntax])) ),
 ( "\x21d2",    (DoubleArrow,   Just (Any  [UnicodeSyntax])) ),
 ( "\x2237",    (DoubleColon,   Just (Any  [UnicodeSyntax])) ),
 ( "\x2605",    (Star,              Just (All [UnicodeSyntax, KindSignatures])) ),
 ( "\x2200",    (KW_Forall,         Just (All [UnicodeSyntax, ExplicitForAll])) )
 ]

special_varops :: [(String,(Token, Maybe ExtScheme))]
special_varops = [
 -- the dot is only a special symbol together with forall, but can still be used as function composition
 ( ".",  (Dot,          Just (Any [ExplicitForAll, ExistentialQuantification])) ),
 ( "-",  (Minus,        Nothing) ),
 ( "!",  (Exclamation,  Nothing) )
 ]

reserved_ids :: [(String,(Token, Maybe ExtScheme))]
reserved_ids = [
 ( "_",         (Underscore,    Nothing) ),
 ( "by",        (KW_By,         Just (Any [TransformListComp])) ),
 ( "case",      (KW_Case,       Nothing) ),
 ( "class",     (KW_Class,      Nothing) ),
 ( "data",      (KW_Data,       Nothing) ),
 ( "default",   (KW_Default,    Nothing) ),
 ( "deriving",  (KW_Deriving,   Nothing) ),
 ( "do",        (KW_Do,         Nothing) ),
 ( "else",      (KW_Else,       Nothing) ),
 ( "family",    (KW_Family,     Just (Any [TypeFamilies])) ),        -- indexed type families
 ( "forall",    (KW_Forall,     Just (Any [ExplicitForAll, ExistentialQuantification])) ),    -- universal/existential quantification
 ( "fun",       (KW_Fun,        Nothing) ),
 ( "if",        (KW_If,         Nothing) ),
 ( "import",    (KW_Import,     Nothing) ),
 ( "in",        (KW_In,         Nothing) ),
 ( "infix",     (KW_Infix,      Nothing) ),
 ( "infixl",    (KW_InfixL,     Nothing) ),
 ( "infixr",    (KW_InfixR,     Nothing) ),
 ( "instance",  (KW_Instance,   Nothing) ),
 ( "let",       (KW_Let,        Nothing) ),
 ( "mdo",       (KW_MDo,        Just (Any [RecursiveDo])) ),
 ( "module",    (KW_Module,     Nothing) ),
 ( "newtype",   (KW_NewType,    Nothing) ),
 ( "of",        (KW_Of,         Nothing) ),
 ( "then",      (KW_Then,       Nothing) ),
 ( "type",      (KW_Type,       Nothing) ),
 ( "where",     (KW_Where,      Nothing) ),
 ( "pattern",   (KW_Pattern,    Just (Any [PatternSynonyms]))),
 ( "stock",     (KW_Stock,      Just (Any [DerivingStrategies]))),
 ( "anyclass",  (KW_Anyclass,   Just (Any [DerivingStrategies]))),
 ( "via",       (KW_Via,        Just (Any [DerivingVia]))),

-- FFI
 ( "foreign",   (KW_Foreign,    Just (Any [ForeignFunctionInterface])) )
 ]


special_varids :: [(String,(Token, Maybe ExtScheme))]
special_varids = [
 ( "as",        (KW_As,         Nothing) ),
 ( "qualified", (KW_Qualified,  Nothing) ),
 ( "hiding",    (KW_Hiding,     Nothing) ),

-- FFI
 ( "export",        (KW_Export,        Just (Any [ForeignFunctionInterface])) ),
 ( "stdcall",       (KW_StdCall,       Just (Any [ForeignFunctionInterface])) ),
 ( "ccall",         (KW_CCall,         Just (Any [ForeignFunctionInterface])) ),
 ( "cplusplus",     (KW_CPlusPlus,     Just (Any [ForeignFunctionInterface])) ),
 ( "dotnet",        (KW_DotNet,        Just (Any [ForeignFunctionInterface])) ),
 ( "jvm",           (KW_Jvm,           Just (Any [ForeignFunctionInterface])) ),
 ( "js",            (KW_Js,            Just (Any [ForeignFunctionInterface])) ),
 ( "javascript",    (KW_JavaScript,    Just (Any [ForeignFunctionInterface])) ),
 ( "capi",          (KW_CApi,          Just (Any [CApiFFI])) )
 ]

pragmas :: [(String,Token)]
pragmas = [
 ( "rules",             RULES           ),
 ( "inline",            INLINE True     ),
 ( "noinline",          INLINE False    ),
 ( "notinline",         INLINE False    ),
 ( "specialise",        SPECIALISE      ),
 ( "specialize",        SPECIALISE      ),
 ( "source",            SOURCE          ),
 ( "deprecated",        DEPRECATED      ),
 ( "warning",           WARNING         ),
 ( "ann",               ANN             ),
 ( "scc",               SCC             ),
 ( "generated",         GENERATED       ),
 ( "core",              CORE            ),
 ( "unpack",            UNPACK          ),
 ( "nounpack",          NOUNPACK        ),
 ( "language",          LANGUAGE        ),
 ( "minimal",           MINIMAL         ),
 ( "no_overlap",        NO_OVERLAP      ),
 ( "overlap",           OVERLAP         ),
 ( "overlaps",          OVERLAPS        ),
 ( "overlapping",       OVERLAPPING     ),
 ( "overlappable",      OVERLAPPABLE    ),
 ( "incoherent",        INCOHERENT      ),
 ( "complete",          COMPLETE      ),
 ( "options",           OPTIONS undefined ) -- we'll tweak it before use - promise!
-- ( "cfiles",            CFILES  undefined ), -- same here...
-- ( "include",           INCLUDE undefined )  -- ...and here!
 ]

isIdent, isHSymbol, isPragmaChar :: Char -> Bool
isIdent   c = isAlphaNum c || c == '\'' || c == '_'

isHSymbol c = c `elem` ":!#%&*./?@\\-" || ((isSymbol c || isPunctuation c) && not (c `elem` "(),;[]`{}_\"'"))

isPragmaChar c = isAlphaNum c || c == '_'

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c && not (isUpper c) || c == '_'


-- Used in the lexing of type applications
-- Why is it like this? I don't know exactly but this is how it is in
-- GHC's parser.
isOpSymbol :: Char -> Bool
isOpSymbol c = c `elem` "!#$%&*+./<=>?@\\^|-~"

-- | Checks whether the character would be legal in some position of a qvar.
--   Means that '..' and "AAA" will pass the test.
isPossiblyQvar :: Char -> Bool
isPossiblyQvar c = isIdent (toLower c) || c == '.'

matchChar :: Char -> String -> Lex a ()
matchChar c msg = do
    s <- getInput
    if null s || head s /= c then fail msg else discard 1

-- The top-level lexer.
-- We need to know whether we are at the beginning of the line to decide
-- whether to insert layout tokens.

lexer :: (Loc Token -> P a) -> P a
lexer = runL topLexer

topLexer :: Lex a (Loc Token)
topLexer = do
    b <- pullCtxtFlag
    if b then -- trace (show cf ++ ": " ++ show VRightCurly) $
              -- the lex context state flags that we must do an empty {} - UGLY
              setBOL >> getSrcLocL >>= \l -> return (Loc (mkSrcSpan l l) VRightCurly)
     else do
        bol <- checkBOL
        (bol', ws) <- lexWhiteSpace bol
        -- take care of whitespace in PCDATA
        ec <- getExtContext
        case ec of
         -- if there was no linebreak, and we are lexing PCDATA,
         -- then we want to care about the whitespace.
         -- We don't bother to test for XmlSyntax, since we
         -- couldn't end up in ChildCtxt otherwise.
         -- Just ChildCtxt | not bol' && ws -> getSrcLocL >>= \l -> return $ Loc (mkSrcSpan l l) $ XPCDATA " "
         _ -> do startToken
                 sl <- getSrcLocL
                 t <- if bol' then lexBOL    -- >>= \t -> trace ("BOL: " ++ show t) (return t)
                              else lexToken  -- >>= \t -> trace (show t) (return t)
                 el <- getSrcLocL
                 return $ Loc (mkSrcSpan sl el) t

lexWhiteSpace :: Bool -> Lex a (Bool, Bool)
lexWhiteSpace bol = do
    s <- getInput
    ignL <- ignoreLinePragmasL
    case s of
        -- If we find a recognised pragma, we don't want to treat it as a comment.
        '{':'-':'#':rest | isRecognisedPragma rest -> return (bol, False)
                         | isLinePragma rest && not ignL -> do
                            (l, fn) <- lexLinePragma
                            setSrcLineL l
                            setLineFilenameL fn
                            lexWhiteSpace True
        '{':'-':_ -> do
            loc <- getSrcLocL
            discard 2
            (bol1, c) <- lexNestedComment bol ""
            loc2 <- getSrcLocL
            pushComment $ Comment True (mkSrcSpan loc loc2) (reverse c)
            (bol2, _) <- lexWhiteSpace bol1
            return (bol2, True)
        '-':'-':s1 | all (== '-') (takeWhile isHSymbol s1) -> do
            loc    <- getSrcLocL
            discard 2
            dashes <- lexWhile (== '-')
            rest   <- lexWhile (/= '\n')
            s' <- getInput
            loc2 <- getSrcLocL
            let com = Comment False (mkSrcSpan loc loc2) $ dashes ++ rest
            case s' of
                [] -> pushComment com >> return (False, True)
                _ -> do
                    pushComment com
                    lexNewline
                    lexWhiteSpace_ True
                    return (True, True)
        '\n':_ -> do
            lexNewline
            lexWhiteSpace_ True
            return (True, True)
        '\t':_ -> do
            lexTab
            (bol', _) <- lexWhiteSpace bol
            return (bol', True)
        c:_ | isSpace c -> do
            discard 1
            (bol', _) <- lexWhiteSpace bol
            return (bol', True)
        _ -> return (bol, False)

-- | lexWhiteSpace without the return value.
lexWhiteSpace_ :: Bool -> Lex a ()
lexWhiteSpace_ bol =  do _ <- lexWhiteSpace bol
                         return ()

isRecognisedPragma, isLinePragma :: String -> Bool
isRecognisedPragma str = let pragma = takeWhile isPragmaChar . dropWhile isSpace $ str
                          in case lookupKnownPragma pragma of
                              Nothing -> False
                              _       -> True

isLinePragma str = let pragma = map toLower . takeWhile isAlphaNum . dropWhile isSpace $ str
                    in case pragma of
                        "line"  -> True
                        _       -> False

lexLinePragma :: Lex a (Int, String)
lexLinePragma = do
    discard 3   -- {-#
    lexWhile_ isSpace
    discard 4   -- LINE
    lexWhile_ isSpace
    i <- lexWhile isDigit
    when (null i) $ fail "Improperly formatted LINE pragma"
    lexWhile_ isSpace
    matchChar '"' "Improperly formatted LINE pragma"
    fn <- lexWhile (/= '"')
    matchChar '"' "Impossible - lexLinePragma"
    lexWhile_ isSpace
    mapM_ (flip matchChar "Improperly formatted LINE pragma") "#-}"
    lexNewline
    return (read i, fn)

lexNestedComment :: Bool -> String -> Lex a (Bool, String)
lexNestedComment bol str = do
    s <- getInput
    case s of
        '-':'}':_ -> discard 2 >> return (bol, str)
        '{':'-':_ -> do
            discard 2
            (bol', c) <- lexNestedComment bol ("-{" ++ str) -- rest of the subcomment
            lexNestedComment bol' ("}-" ++ c  ) -- rest of this comment
        '\t':_    -> lexTab >> lexNestedComment bol ('\t':str)
        '\n':_    -> lexNewline >> lexNestedComment True ('\n':str)
        c:_       -> discard 1 >> lexNestedComment bol (c:str)
        []        -> fail "Unterminated nested comment"

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: Lex a Token
lexBOL = do
    pos <- getOffside
    -- trace ("Off: " ++ (show pos)) $ do
    case pos of
        LT -> do
                -- trace "layout: inserting '}'\n" $
            -- Set col to 0, indicating that we're still at the
            -- beginning of the line, in case we need a semi-colon too.
            -- Also pop the context here, so that we don't insert
            -- another close brace before the parser can pop it.
            setBOL
            popContextL "lexBOL"
            return VRightCurly
        EQ ->
            -- trace "layout: inserting ';'\n" $
            return SemiColon
        GT -> lexToken

lexToken :: Lex a Token
lexToken = do
    ec <- getExtContext
    -- we don't bother to check XmlSyntax since we couldn't
    -- have ended up in a non-Nothing context if it wasn't
    -- enabled.
    case ec of _ -> lexStdToken

lexStdToken :: Lex a Token
lexStdToken = do
    s <- getInput
    exts <- getExtensionsL
    let intHash = lexHash IntTok IntTokHash (Right WordTokHash)
    case s of
        [] -> return EOF

        '0':c:d:_ | toLower c == 'o' && isOctDigit d -> do
                        discard 2
                        (n, str) <- lexOctal
                        con <- intHash
                        return (con (n, '0':c:str))
                  | toLower c == 'b' && isBinDigit d && BinaryLiterals `elem` exts -> do
                        discard 2
                        (n, str) <- lexBinary
                        con <- intHash
                        return (con (n, '0':c:str))
                  | toLower c == 'x' && isHexDigit d -> do
                        discard 2
                        (n, str) <- lexHexadecimal
                        con <- intHash
                        return (con (n, '0':c:str))

        -- implicit parameters
        '?':c:_ | isIdentStart c && ImplicitParams `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ IDupVarId id

        '%':c:_ | isIdentStart c && ImplicitParams `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ ILinVarId id
        -- end implicit parameters

        -- pragmas

        '{':'-':'#':_ -> saveExtensionsL >> discard 3 >> lexPragmaStart

        '#':'-':'}':_ -> restoreExtensionsL >> discard 3 >> return PragmaEnd

        '#':c:_ | OverloadedLabels `elem` exts
                   && isIdentStart c -> do
                                                  discard 1
                                                  [ident] <- lexIdents
                                                  return $ LabelVarId ident


        c:_ | isDigit c -> lexDecimalOrFloat

            | isUpper c -> lexConIdOrQual ""

            | isIdentStart c -> do
                    idents <- lexIdents
                    case idents of
                     [ident] -> case lookup ident (reserved_ids ++ special_varids) of
                                 Just (keyword, scheme) ->
                                    -- check if an extension keyword is enabled
                                    if isEnabled scheme exts
                                     then flagKW keyword >> return keyword
                                     else return $ VarId ident
                                 Nothing -> return $ VarId ident
                     _ -> return $ DVarId idents

            | isHSymbol c -> do
                    sym <- lexWhile isHSymbol
                    return $ case lookup sym (reserved_ops ++ special_varops) of
                              Just (t , scheme) ->
                                -- check if an extension op is enabled
                                if isEnabled scheme exts
                                 then t
                                 else case c of
                                        ':' -> ConSym sym
                                        _   -> VarSym sym
                              Nothing -> case c of
                                          ':' -> ConSym sym
                                          _   -> VarSym sym

            | otherwise -> do
                    discard 1
                    case c of

                        -- First the special symbols
                        '(' ->  return LeftParen
                        ')' ->  return RightParen
                        ',' ->  return Comma
                        ';' ->  return SemiColon
                        '[' ->  return LeftSquare
                        ']' ->  return RightSquare
                        '`' ->  return BackQuote
                        '{' -> do
                            pushContextL NoLayout
                            return LeftCurly
                        '}' -> do
                            popContextL "lexStdToken"
                            return RightCurly

                        '\'' -> lexCharacter
                        '"' ->  lexString

                        _ ->    fail ("Illegal character \'" ++ show c ++ "\'\n")

      where lexIdents :: Lex a [String]
            lexIdents = do
                ident <- lexWhile isIdent
                return [ident]

-- Underscores are used in some pragmas. Options pragmas are a special case
-- with our representation: the thing after the underscore is a parameter.
-- Strip off the parameters to option pragmas by hand here, everything else
-- sits in the pragmas map.
lookupKnownPragma :: String -> Maybe Token
lookupKnownPragma s =
    case map toLower s of
      x | "options_" `isPrefixOf` x -> Just $ OPTIONS (Just $ drop 8 s, undefined)
        | "options" == x            -> Just $ OPTIONS (Nothing, undefined)
        | otherwise                 -> lookup x pragmas

lexPragmaStart :: Lex a Token
lexPragmaStart = do
    lexWhile_ isSpace
    pr <- lexWhile isPragmaChar
    case lookupKnownPragma pr of
     Just (INLINE True) -> do
            s <- getInput
            case map toLower s of
             ' ':'c':'o':'n':'l':'i':'k':'e':_  -> do
                      discard 8
                      return INLINE_CONLIKE
             _ -> return $ INLINE True
     Just SPECIALISE -> do
            s <- getInput
            case dropWhile isSpace $ map toLower s of
             'i':'n':'l':'i':'n':'e':_ -> do
                      lexWhile_ isSpace
                      discard 6
                      return $ SPECIALISE_INLINE True
             'n':'o':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile_ isSpace
                        discard 8
                        return $ SPECIALISE_INLINE False
             'n':'o':'t':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile_ isSpace
                        discard 9
                        return $ SPECIALISE_INLINE False
             _ -> return SPECIALISE

     Just (OPTIONS opt) ->     -- see, I promised we'd mask out the 'undefined'
            -- We do not want to store necessary whitespace in the datatype
            -- but if the pragma starts with a newline then we must keep
            -- it to differentiate the two cases.
            let dropIfSpace (' ':xs) = xs
                dropIfSpace xs       = xs
             in
              case fst opt of
                Just opt' -> do
                  rest <- lexRawPragma
                  return $ OPTIONS (Just opt', dropIfSpace rest)
                Nothing -> do
                  s <- getInput
                  case s of
                    x:_ | isSpace x -> do
                      rest <- lexRawPragma
                      return $ OPTIONS (Nothing, dropIfSpace rest)
                    _  -> fail "Malformed Options pragma"
     Just RULES -> do -- Rules enable ScopedTypeVariables locally.
            addExtensionL ScopedTypeVariables
            return RULES
{-     Just (CFILES _) -> do
            rest <- lexRawPragma
            return $ CFILES rest
     Just (INCLUDE _) -> do
            rest <- lexRawPragma
            return $ INCLUDE rest -}
     Just p ->  return p

     _      -> fail "Internal error: Unrecognised recognised pragma"
                  -- do rawStr <- lexRawPragma
                  -- return $ PragmaUnknown (pr, rawStr) -- no support for unrecognized pragmas, treat as comment
                  -- discard 3 -- #-}
                  -- topLexer -- we just discard it as a comment for now and restart -}

lexRawPragma :: Lex a String
lexRawPragma = lexRawPragmaAux
 where lexRawPragmaAux = do
        rpr <- lexWhile (/='#')
        s <- getInput
        case s of
         '#':'-':'}':_  -> return rpr
         "" -> fail "End-of-file inside pragma"
         _ -> do
            discard 1
            rpr' <- lexRawPragma
            return $ rpr ++ '#':rpr'

lexDecimalOrFloat :: Lex a Token
lexDecimalOrFloat = do
    ds <- lexWhile isDigit
    rest <- getInput
    exts <- getExtensionsL
    case rest of
        ('.':d:_) | isDigit d -> do
                discard 1
                frac <- lexWhile isDigit
                let num = parseInteger 10 (ds ++ frac)
                    decimals = toInteger (length frac)
                (exponent, estr) <- do
                    rest2 <- getInput
                    case rest2 of
                        'e':_ -> lexExponent
                        'E':_ -> lexExponent
                        _     -> return (0,"")
                con <- lexHash FloatTok FloatTokHash (Right DoubleTokHash)
                return $ con ((num%1) * 10^^(exponent - decimals), ds ++ '.':frac ++ estr)
        e:_ | toLower e == 'e' -> do
                (exponent, estr) <- lexExponent
                con <- lexHash FloatTok FloatTokHash (Right DoubleTokHash)
                return $ con ((parseInteger 10 ds%1) * 10^^exponent, ds ++ estr)
        _         ->              return (IntTok      (parseInteger 10 ds, ds))

    where
    lexExponent :: Lex a (Integer, String)
    lexExponent = do
        (e:r) <- getInput
        discard 1   -- 'e' or 'E'
        case r of
         '+':d:_ | isDigit d -> do
            discard 1
            (n, str) <- lexDecimal
            return (n, e:'+':str)
         '-':d:_ | isDigit d -> do
            discard 1
            (n, str) <- lexDecimal
            return (negate n, e:'-':str)
         d:_ | isDigit d -> lexDecimal >>= \(n,str) -> return (n, e:str)
         _ -> fail "Float with missing exponent"

lexHash :: (b -> Token) -> (b -> Token) -> Either String (b -> Token) -> Lex a (b -> Token)
lexHash a b c = do return a

lexConIdOrQual :: String -> Lex a Token
lexConIdOrQual qual = do
        con <- lexWhile isIdent
        let conid | null qual = ConId con
                  | otherwise = QConId (qual,con)
            qual' | null qual = con
                  | otherwise = qual ++ '.':con
        just_a_conid <- alternative (return conid)
        rest <- getInput
        exts <- getExtensionsL
        case rest of
          '.':c:_
             | isIdentStart c -> do  -- qualified varid?
                    discard 1
                    ident <- lexWhile isIdent
                    s <- getInput
                    exts' <- getExtensionsL
                    ident' <- case s of
                               _ -> return ident
                    case lookup ident' reserved_ids of
                       -- cannot qualify a reserved word
                       Just (_,scheme) | isEnabled scheme exts'  -> just_a_conid
                       _ -> return (QVarId (qual', ident'))

             | isUpper c -> do      -- qualified conid?
                    discard 1
                    lexConIdOrQual qual'

             | isHSymbol c -> do    -- qualified symbol?
                    discard 1
                    sym <- lexWhile isHSymbol
                    exts' <- getExtensionsL
                    case lookup sym reserved_ops of
                        -- cannot qualify a reserved operator
                        Just (_,scheme) | isEnabled scheme exts' -> just_a_conid
                        _        -> return $ case c of
                                              ':' -> QConSym (qual', sym)
                                              _   -> QVarSym (qual', sym)

          _ ->  return conid -- not a qualified thing

lexCharacter :: Lex a Token
lexCharacter = do   -- We need to keep track of not only character constants but also TH 'x and ''T
        -- We've seen ' so far
        s <- getInput
        -- exts <- getExtensionsL
        case s of
         '\\':_ -> do
                    (c,raw) <- lexEscape
                    matchQuote
                    con <- lexHash Character CharacterHash
                            (Left "Double hash not available for character literals")
                    return (con (c, '\\':raw))
         c:'\'':_ -> do
                    discard 2
                    con <- lexHash Character CharacterHash
                            (Left "Double hash not available for character literals")
                    return (con (c, [c]))
         _ -> fail "Improper character constant or misplaced \'"

    where matchQuote = matchChar '\'' "Improperly terminated character constant"


lexString :: Lex a Token
lexString = loop ("","")
    where
    loop (s,raw) = do
        r <- getInput
        exts <- getExtensionsL
        case r of
            '\\':'&':_ -> do
                    discard 2
                    loop (s, '&':'\\':raw)
            '\\':c:_ | isSpace c -> do
                        discard 1
                        wcs <- lexWhiteChars
                        matchChar '\\' "Illegal character in string gap"
                        loop (s, '\\':reverse wcs ++ '\\':raw)
                     | otherwise -> do
                        (ce, str) <- lexEscape
                        loop (ce:s, reverse str ++ '\\':raw)
            '"':_ -> do
                discard 1
                return (StringTok (reverse s, reverse raw))
            c:_ | c /= '\n' -> do
                discard 1
                loop (c:s, c:raw)
            _ ->   fail "Improperly terminated string"

    lexWhiteChars :: Lex a String
    lexWhiteChars = do
        s <- getInput
        case s of
            '\n':_ -> do
                    lexNewline
                    wcs <- lexWhiteChars
                    return $ '\n':wcs
            '\t':_ -> do
                    lexTab
                    wcs <- lexWhiteChars
                    return $ '\t':wcs
            c:_ | isSpace c -> do
                    discard 1
                    wcs <- lexWhiteChars
                    return $ c:wcs
            _ -> return ""

lexEscape :: Lex a (Char, String)
lexEscape = do
    discard 1
    r <- getInput
    case r of

-- Production charesc from section B.2 (Note: \& is handled by caller)

        'a':_           -> discard 1 >> return ('\a', "a")
        'b':_           -> discard 1 >> return ('\b', "b")
        'f':_           -> discard 1 >> return ('\f', "f")
        'n':_           -> discard 1 >> return ('\n', "n")
        'r':_           -> discard 1 >> return ('\r', "r")
        't':_           -> discard 1 >> return ('\t', "t")
        'v':_           -> discard 1 >> return ('\v', "v")
        '\\':_          -> discard 1 >> return ('\\', "\\")
        '"':_           -> discard 1 >> return ('\"', "\"")
        '\'':_          -> discard 1 >> return ('\'', "\'")

-- Production ascii from section B.2

        '^':c:_         -> discard 2 >> cntrl c
        'N':'U':'L':_   -> discard 3 >> return ('\NUL', "NUL")
        'S':'O':'H':_   -> discard 3 >> return ('\SOH', "SOH")
        'S':'T':'X':_   -> discard 3 >> return ('\STX', "STX")
        'E':'T':'X':_   -> discard 3 >> return ('\ETX', "ETX")
        'E':'O':'T':_   -> discard 3 >> return ('\EOT', "EOT")
        'E':'N':'Q':_   -> discard 3 >> return ('\ENQ', "ENQ")
        'A':'C':'K':_   -> discard 3 >> return ('\ACK', "ACK")
        'B':'E':'L':_   -> discard 3 >> return ('\BEL', "BEL")
        'B':'S':_       -> discard 2 >> return ('\BS',  "BS")
        'H':'T':_       -> discard 2 >> return ('\HT',  "HT")
        'L':'F':_       -> discard 2 >> return ('\LF',  "LF")
        'V':'T':_       -> discard 2 >> return ('\VT',  "VT")
        'F':'F':_       -> discard 2 >> return ('\FF',  "FF")
        'C':'R':_       -> discard 2 >> return ('\CR',  "CR")
        'S':'O':_       -> discard 2 >> return ('\SO',  "SO")
        'S':'I':_       -> discard 2 >> return ('\SI',  "SI")
        'D':'L':'E':_   -> discard 3 >> return ('\DLE', "DLE")
        'D':'C':'1':_   -> discard 3 >> return ('\DC1', "DC1")
        'D':'C':'2':_   -> discard 3 >> return ('\DC2', "DC2")
        'D':'C':'3':_   -> discard 3 >> return ('\DC3', "DC3")
        'D':'C':'4':_   -> discard 3 >> return ('\DC4', "DC4")
        'N':'A':'K':_   -> discard 3 >> return ('\NAK', "NAK")
        'S':'Y':'N':_   -> discard 3 >> return ('\SYN', "SYN")
        'E':'T':'B':_   -> discard 3 >> return ('\ETB', "ETB")
        'C':'A':'N':_   -> discard 3 >> return ('\CAN', "CAN")
        'E':'M':_       -> discard 2 >> return ('\EM',  "EM")
        'S':'U':'B':_   -> discard 3 >> return ('\SUB', "SUB")
        'E':'S':'C':_   -> discard 3 >> return ('\ESC', "ESC")
        'F':'S':_       -> discard 2 >> return ('\FS',  "FS")
        'G':'S':_       -> discard 2 >> return ('\GS',  "GS")
        'R':'S':_       -> discard 2 >> return ('\RS',  "RS")
        'U':'S':_       -> discard 2 >> return ('\US',  "US")
        'S':'P':_       -> discard 2 >> return ('\SP',  "SP")
        'D':'E':'L':_   -> discard 3 >> return ('\DEL', "DEL")

-- Escaped numbers

        'o':c:_ | isOctDigit c -> do
                    discard 1
                    (n, raw) <- lexOctal
                    n' <- checkChar n
                    return (n', 'o':raw)
        'x':c:_ | isHexDigit c -> do
                    discard 1
                    (n, raw) <- lexHexadecimal
                    n' <- checkChar n
                    return (n', 'x':raw)
        c:_ | isDigit c -> do
                    (n, raw) <- lexDecimal
                    n' <- checkChar n
                    return (n', raw)

        _       -> fail "Illegal escape sequence"

    where
    checkChar n | n <= 0x10FFFF = return (chr (fromInteger n))
    checkChar _                 = fail "Character constant out of range"

-- Production cntrl from section B.2

    cntrl :: Char -> Lex a (Char, String)
    cntrl c | c >= '@' && c <= '_' = return (chr (ord c - ord '@'), '^':c:[])
    cntrl _                        = fail "Illegal control character"

-- assumes at least one octal digit
lexOctal :: Lex a (Integer, String)
lexOctal = do
    ds <- lexWhile isOctDigit
    return (parseInteger 8 ds, ds)

-- assumes at least one binary digit
lexBinary :: Lex a (Integer, String)
lexBinary = do
    ds <- lexWhile isBinDigit
    return (parseInteger 2 ds, ds)

-- assumes at least one hexadecimal digit
lexHexadecimal :: Lex a (Integer, String)
lexHexadecimal = do
    ds <- lexWhile isHexDigit
    return (parseInteger 16 ds, ds)

-- assumes at least one decimal digit
lexDecimal :: Lex a (Integer, String)
lexDecimal = do
    ds <- lexWhile isDigit
    return (parseInteger 10 ds, ds)

-- Stolen from Hugs's Prelude
parseInteger :: Integer -> String -> Integer
parseInteger radix ds =
    foldl1 (\n d -> n * radix + d) (map (toInteger . digitToInt) ds)

flagKW :: Token -> Lex a ()
flagKW t =
  when (t `elem` [KW_Do, KW_MDo]) $ do
       exts <- getExtensionsL
       when (NondecreasingIndentation `elem` exts) flagDo

-- | Selects ASCII binary digits, i.e. @\'0\'@..@\'1\'@.
isBinDigit :: Char -> Bool
isBinDigit c =  c >= '0' && c <= '1'
------------------------------------------------------------------
-- "Pretty" printing for tokens

showToken :: Token -> String
showToken t = case t of
  VarId s           -> s
  LabelVarId s      -> '#':s
  QVarId (q,s)      -> q ++ '.':s
  IDupVarId s       -> '?':s
  ILinVarId s       -> '%':s
  ConId s           -> s
  QConId (q,s)      -> q ++ '.':s
  DVarId ss         -> intercalate "-" ss
  VarSym s          -> s
  ConSym s          -> s
  QVarSym (q,s)     -> q ++ '.':s
  QConSym (q,s)     -> q ++ '.':s
  IntTok (_, s)         -> s
  FloatTok (_, s)       -> s
  Character (_, s)      -> '\'':s ++ "'"
  StringTok (_, s)      -> '"':s ++ "\""
  IntTokHash (_, s)     -> s ++ "#"
  WordTokHash (_, s)    -> s ++ "##"
  FloatTokHash (_, s)   -> s ++ "#"
  DoubleTokHash (_, s)  -> s ++ "##"
  CharacterHash (_, s)  -> '\'':s ++ "'#"
  StringHash (_, s)     -> '"':s ++ "\"#"
  LeftParen         -> "("
  RightParen        -> ")"
  LeftHashParen     -> "(#"
  RightHashParen    -> "#)"
  SemiColon         -> ";"
  LeftCurly         -> "{"
  RightCurly        -> "}"
  VRightCurly       -> "virtual }"
  LeftSquare        -> "["
  RightSquare       -> "]"
  Comma             -> ","
  Underscore        -> "_"
  BackQuote         -> "`"
  QuoteColon        -> "':"
  Dot               -> "."
  DotDot            -> ".."
  Colon             -> ":"
  DoubleColon       -> "::"
  Equals            -> "="
  Backslash         -> "\\"
  Bar               -> "|"
  LeftArrow         -> "<-"
  RightArrow        -> "->"
  At                -> "@"
  TApp              -> "@"
  Tilde             -> "~"
  DoubleArrow       -> "=>"
  Minus             -> "-"
  Exclamation       -> "!"
  Star              -> "*"
  PragmaEnd         -> "#-}"
  RULES             -> "{-# RULES"
  INLINE b          -> "{-# " ++ if b then "INLINE" else "NOINLINE"
  INLINE_CONLIKE    -> "{-# " ++ "INLINE CONLIKE"
  SPECIALISE        -> "{-# SPECIALISE"
  SPECIALISE_INLINE b -> "{-# SPECIALISE " ++ if b then "INLINE" else "NOINLINE"
  SOURCE            -> "{-# SOURCE"
  DEPRECATED        -> "{-# DEPRECATED"
  WARNING           -> "{-# WARNING"
  SCC               -> "{-# SCC"
  GENERATED         -> "{-# GENERATED"
  CORE              -> "{-# CORE"
  UNPACK            -> "{-# UNPACK"
  NOUNPACK          -> "{-# NOUNPACK"
  OPTIONS (mt,_)    -> "{-# OPTIONS" ++ maybe "" (':':) mt ++ " ..."
--  CFILES  s         -> "{-# CFILES ..."
--  INCLUDE s         -> "{-# INCLUDE ..."
  LANGUAGE          -> "{-# LANGUAGE"
  ANN               -> "{-# ANN"
  MINIMAL           -> "{-# MINIMAL"
  NO_OVERLAP        -> "{-# NO_OVERLAP"
  OVERLAP           -> "{-# OVERLAP"
  OVERLAPPING       -> "{-# OVERLAPPING"
  OVERLAPPABLE      -> "{-# OVERLAPPABLE"
  OVERLAPS          -> "{-# OVERLAPS"
  INCOHERENT        -> "{-# INCOHERENT"
  COMPLETE          -> "{-# COMPLETE"
  KW_As         -> "as"
  KW_By         -> "by"
  KW_Case       -> "case"
  KW_Class      -> "class"
  KW_Data       -> "data"
  KW_Default    -> "default"
  KW_Deriving   -> "deriving"
  KW_Do         -> "do"
  KW_MDo        -> "mdo"
  KW_Else       -> "else"
  KW_Family     -> "family"
  KW_Forall     -> "forall"
  KW_Fun        -> "fun"
  KW_Hiding     -> "hiding"
  KW_If         -> "if"
  KW_Import     -> "import"
  KW_In         -> "in"
  KW_Infix      -> "infix"
  KW_InfixL     -> "infixl"
  KW_InfixR     -> "infixr"
  KW_Instance   -> "instance"
  KW_Let        -> "let"
  KW_Module     -> "module"
  KW_NewType    -> "newtype"
  KW_Of         -> "of"
  KW_Then       -> "then"
  KW_Type       -> "type"
  KW_Where      -> "where"
  KW_Qualified  -> "qualified"
  KW_Foreign    -> "foreign"
  KW_Export     -> "export"
  KW_StdCall    -> "stdcall"
  KW_CCall      -> "ccall"
  KW_CPlusPlus  -> "cplusplus"
  KW_DotNet     -> "dotnet"
  KW_Jvm        -> "jvm"
  KW_Js         -> "js"
  KW_JavaScript -> "javascript"
  KW_CApi       -> "capi"
  KW_Pattern    -> "pattern"
  KW_Stock      -> "stock"
  KW_Anyclass   -> "anyclass"
  KW_Via        -> "via"

  EOF           -> "EOF"
