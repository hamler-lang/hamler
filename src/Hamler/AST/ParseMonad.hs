{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.ParseMonad
-- Copyright   :  Niklas Broberg (c) 2004-2009,
--                Original (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Monads for the Haskell parser and lexer.
--
-----------------------------------------------------------------------------

module Hamler.AST.ParseMonad(
        -- * Generic Parsing
        Parseable(..),
        -- * Parsing
        P, ParseResult(..), atSrcLoc, LexContext(..),
        ParseMode(..), defaultParseMode, fromParseResult,
        runParserWithMode, runParserWithModeComments, runParser,
        getSrcLoc, pushCurrentContext, popContext,
        getExtensions, getIgnoreFunctionArity,
        -- * Lexing
        Lex(runL), getInput, discard, getLastChar, lexNewline,
        lexTab, lexWhile, lexWhile_,
        alternative, checkBOL, setBOL, startToken, getOffside,
        pushContextL, popContextL, getExtensionsL, addExtensionL,
        saveExtensionsL, restoreExtensionsL, pushComment,
        getSrcLocL, setSrcLineL, ignoreLinePragmasL, setLineFilenameL,
        -- * Harp/Hsx
        ExtContext(..),
        pushExtContextL, popExtContextL, getExtContext,
        pullCtxtFlag, flagDo,
        getModuleName
    ) where

import Hamler.AST.SrcLoc (SrcLoc(..), noLoc)
import Hamler.AST.Fixity (Fixity, preludeFixities)
import Hamler.AST.Comments
import Hamler.AST.Extension -- (Extension, impliesExts, haskell2010)

import Data.List (intercalate)
import Control.Applicative
import Control.Monad (when, liftM, ap)
import qualified Control.Monad.Fail as Fail
import Data.Monoid hiding ((<>))
import Data.Semigroup (Semigroup(..))
-- To avoid import warnings for Control.Applicative, Data.Monoid, and Data.Semigroup
import Prelude

-- | Class providing function for parsing at many different types.
--
--   Note that for convenience of implementation, the default methods have
--   definitions equivalent to 'undefined'.  The minimal definition is all of
--   the visible methods.
class Parseable ast where
  -- | Parse a string with default mode.
  parse :: String -> ParseResult ast
  parse = parseWithMode defaultParseMode
  -- | Parse a string with an explicit 'ParseMode'.
  parseWithMode :: ParseMode -> String -> ParseResult ast
  parseWithMode mode = runParserWithMode mode . parser $ fixities mode
  -- | Parse a string with an explicit 'ParseMode', returning all comments along
  --   with the AST.
  parseWithComments :: ParseMode -> String -> ParseResult (ast, [Comment])
  parseWithComments mode = runParserWithModeComments mode . parser $ fixities mode
  -- | Internal parser, used to provide default definitions for the others.
  parser :: Maybe [Fixity] -> P ast

-- | The result of a parse.
data ParseResult a
    = ParseOk a  -- ^ The parse succeeded, yielding a value.
    | ParseFailed SrcLoc String
                -- ^ The parse failed at the specified
                -- source location, with an error message.
    deriving (Show, Ord, Eq)

-- | Retrieve the result of a successful parse, throwing an
--   error if the parse is actually not successful.
fromParseResult :: ParseResult a -> a
fromParseResult (ParseOk a) = a
fromParseResult (ParseFailed loc str) = error $ "fromParseResult: Parse failed at ["
                ++ srcFilename loc ++ "] (" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ "): " ++ str

instance Functor ParseResult where
  fmap f (ParseOk x)           = ParseOk $ f x
  fmap _ (ParseFailed loc msg) = ParseFailed loc msg

instance Applicative ParseResult where
  pure = ParseOk
  ParseOk f           <*> x = f <$> x
  ParseFailed loc msg <*> _ = ParseFailed loc msg

instance Monad ParseResult where
  return = ParseOk
#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif
  ParseOk x           >>= f = f x
  ParseFailed loc msg >>= _ = ParseFailed loc msg
instance Fail.MonadFail ParseResult where
  fail = ParseFailed noLoc

instance Semigroup m => Semigroup (ParseResult m) where
 ParseOk x <> ParseOk y = ParseOk $ x <> y
 ParseOk _ <> err       = err
 err       <> _         = err -- left-biased

instance ( Monoid m , Semigroup m) => Monoid (ParseResult m) where
  mempty = ParseOk mempty
  mappend = (<>)

-- internal version
data ParseStatus a = Ok ParseState a | Failed SrcLoc String
    deriving Show

data LexContext = NoLayout | Layout Int
    deriving (Eq,Ord,Show)

data ExtContext = CodeCtxt | HarpCtxt | TagCtxt | ChildCtxt
        | CloseTagCtxt | CodeTagCtxt
    deriving (Eq,Ord,Show)

type CtxtFlag = (Bool,Bool)
-- (True,_) = We're in a do context.
-- (_, True)= Next token must be a virtual closing brace.

type ParseState = ([LexContext],[[KnownExtension]],[ExtContext],CtxtFlag,[Comment])

indentOfParseState :: ParseState -> Int
indentOfParseState (Layout n:_,_,_,_,_) = n
indentOfParseState _                    = 0

-- | Static parameters governing a parse.
--   Note that the various parse functions in "Hamler.AST.Parser"
--   never look at LANGUAGE pragmas, regardless of
--   what the @ignoreLanguagePragmas@ flag is set to.
--   Only the various @parseFile@ functions in "Hamler.Parser" will
--   act on it, when set to 'False'.

data ParseMode = ParseMode {
        -- | original name of the file being parsed
        parseFilename :: String,
        -- | base language (e.g. Haskell98, Haskell2010)
        baseLanguage :: Language,
        -- | list of extensions enabled for parsing
        extensions :: [Extension],
        -- | if 'True', the parser won't care about further extensions
        --   in LANGUAGE pragmas in source files
        ignoreLanguagePragmas :: Bool,
        -- | if 'True', the parser won't read line position information
        --   from LINE pragmas in source files
        ignoreLinePragmas :: Bool,
        -- | list of fixities to be aware of
        fixities :: Maybe [Fixity],
        -- | Checks whether functions have a consistent arity
        ignoreFunctionArity :: Bool
        }

-- | Default parameters for a parse.
--   The default is an unknown filename,
--   no extensions (i.e. Haskell 98),
--   don't ignore LANGUAGE pragmas, do ignore LINE pragmas,
--   and be aware of fixities from the 'Prelude'.
defaultParseMode :: ParseMode
defaultParseMode = ParseMode {
        parseFilename = "<unknown>.hs",
        baseLanguage = Haskell2010,
        extensions = [],
        ignoreLanguagePragmas = False,
        ignoreLinePragmas = True,
        fixities = Just preludeFixities,
        ignoreFunctionArity = False
        }

-- Version of ParseMode used internally,
-- where the language and extensions have
-- been expanded
data InternalParseMode = IParseMode {
        iParseFilename :: String,
        iExtensions :: [KnownExtension],
        -- iIgnoreLanguagePragmas :: Bool,
        iIgnoreLinePragmas :: Bool,
        iIgnoreFunctionArity :: Bool
        -- iFixities :: Maybe [Fixity]
    }

toInternalParseMode :: ParseMode -> InternalParseMode
toInternalParseMode (ParseMode pf bLang exts _ilang iline _fx farity) =
    IParseMode pf (toExtensionList bLang exts) {-_ilang -} iline {- _fx -} farity


-- | Monad for parsing

newtype P a = P { runP ::
                String              -- input string
             -> Int                 -- current column
             -> Int                 -- current line
             -> SrcLoc              -- location of last token read
             -> Char                -- Last token read used for lexing TypeApplication UGH
             -> ParseState          -- layout info.
             -> InternalParseMode   -- parse parameters
             -> ParseStatus a
        }

runParserWithMode :: ParseMode -> P a -> String -> ParseResult a
{-runParserWithMode mode (P m) s = case m s 0 1 start ([],[],(False,False),[]) mode of
    Ok _ a -> ParseOk a
    Failed loc msg -> ParseFailed loc msg
    where start = SrcLoc {
        srcFilename = parseFilename mode,
        srcLine = 1,
        srcColumn = 1
    }
-}
runParserWithMode mode pm = fmap fst . runParserWithModeComments mode pm

runParser :: P a -> String -> ParseResult a
runParser = runParserWithMode defaultParseMode

runParserWithModeComments :: ParseMode -> P a -> String -> ParseResult (a, [Comment])
runParserWithModeComments mode = let mode2 = toInternalParseMode mode in \(P m) s ->
  case m s 0 1 start '\n' ([],[],[],(False,False),[]) mode2 of
    Ok (_,_,_,_,cs) a -> ParseOk (a, reverse cs)
    Failed loc msg    -> ParseFailed loc msg
    where start = SrcLoc {
        srcFilename = parseFilename mode,
        srcLine = 1,
        srcColumn = 1
        }
  --        allExts mode@(ParseMode {extensions = es}) = mode { extensions = impliesExts es }

    --      allExts mode = let imode = to

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = return
  (<*>) = ap

instance Monad P where
    return a = P $ \_i _x _y _l _ch s _m -> Ok s a
    P m >>= k = P $ \i x y l ch s mode ->
        case m i x y l ch s mode of
            Failed loc msg -> Failed loc msg
            Ok s' a -> runP (k a) i x y l ch s' mode
#if !MIN_VERSION_base(4,13,0)
    fail   = Fail.fail
#endif

instance Fail.MonadFail P where
    fail s = P $ \_r _col _line loc _ _stk _m -> Failed loc s

atSrcLoc :: P a -> SrcLoc -> P a
P m `atSrcLoc` loc = P $ \i x y _l ch -> m i x y loc ch

getSrcLoc :: P SrcLoc
getSrcLoc = P $ \_i _x _y l _ s _m -> Ok s l

getModuleName :: P String
getModuleName = P $ \_i _x _y _l _ch s m ->
    let fn = iParseFilename m
        mn = intercalate "." $ splitPath fn

        splitPath :: String -> [String]
        splitPath ""   = []
        splitPath str  = let (l,str') = break ('\\'==) str
                          in case str' of
                              []      -> [removeSuffix l]
                              (_:str'') -> l : splitPath str''

        removeSuffix l = reverse $ tail $ dropWhile ('.'/=) $ reverse l

     in Ok s mn

-- Enter a new layout context.  If we are already in a layout context,
-- ensure that the new indent is greater than the indent of that context.
-- (So if the source loc is not to the right of the current indent, an
-- empty list {} will be inserted.)

pushCurrentContext :: P ()
pushCurrentContext = do
    lc <- getSrcLoc
    indent <- currentIndent
    dob <- pullDoStatus
    let loc = srcColumn lc
    when (dob && loc < indent
           || not dob && loc <= indent) pushCtxtFlag
    pushContext (Layout loc)

currentIndent :: P Int
currentIndent = P $ \_r _x _y _ _ stk _mode -> Ok stk (indentOfParseState stk)

pushContext :: LexContext -> P ()
pushContext ctxt =
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
    P $ \_i _x _y _l _ (s, exts, e, p, c) _m -> Ok (ctxt:s, exts, e, p, c) ()

popContext :: P ()
popContext = P $ \_i _x _y loc _ stk _m ->
      case stk of
        (_:s, exts, e, p, c) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $
                          Ok (s, exts, e, p, c) ()
        ([],_,_,_,_)   -> Failed loc "Unexpected }" -- error "Internal error: empty context in popContext"

-- Extension-aware lexing/parsing
getExtensions :: P [KnownExtension]
getExtensions = P $ \_i _x _y _l _ s m ->
    Ok s $ iExtensions m

pushCtxtFlag :: P ()
pushCtxtFlag =
    P $ \_i _x _y _l _ (s, exts, e, (d,c), cs) _m -> case c of
        False -> Ok (s, exts, e, (d,True), cs) ()
        _     -> error "Internal error: context flag already pushed"

pullDoStatus :: P Bool
pullDoStatus = P $ \_i _x _y _l _ (s, exts, e, (d,c), cs) _m -> Ok (s,exts,e,(False,c),cs) d

getIgnoreFunctionArity :: P Bool
getIgnoreFunctionArity = P $ \_i _x _y _l _ s m ->
  Ok s $ iIgnoreFunctionArity m




----------------------------------------------------------------------------
-- Monad for lexical analysis:
-- a continuation-passing version of the parsing monad

newtype Lex r a = Lex { runL :: (a -> P r) -> P r }

instance Functor (Lex r) where
    fmap = liftM

instance Applicative (Lex r) where
    pure = return
    (<*>) = ap

instance Monad (Lex r) where
    return a = Lex $ \k -> k a
    Lex v >>= f = Lex $ \k -> v (\a -> runL (f a) k)
    Lex v >> Lex w = Lex $ \k -> v (\_ -> w k)
#if !MIN_VERSION_base(4,13,0)
    fail   = Fail.fail
#endif

instance Fail.MonadFail (Lex r) where
    fail s = Lex $ \_ -> fail s

-- Operations on this monad

getInput :: Lex r String
getInput = Lex $ \cont -> P $ \r -> runP (cont r) r

-- | Discard some input characters (these must not include tabs or newlines).

discard :: Int -> Lex r ()
discard n = Lex $ \cont -> P $ \r x y loc ch
                        -> let (newCh:rest)= if n > 0 then drop (n-1) r else (ch:r)
                           in runP (cont ()) rest (x+n) y loc newCh

-- | Get the last discarded character.
-- This is only used for type application.

getLastChar :: Lex r Char
getLastChar = Lex $ \cont -> P $ \r x y loc ch -> runP (cont ch) r x y loc ch


-- | Discard the next character, which must be a newline.

lexNewline :: Lex a ()
lexNewline = Lex $ \cont -> P $ \rs _x y loc  ->
  case rs of
    (_:r) -> runP (cont ()) r 1 (y+1) loc
    []    -> \_ _ _ -> Failed loc "Lexer: expected newline."

-- | Discard the next character, which must be a tab.

lexTab :: Lex a ()
lexTab = Lex $ \cont -> P $ \(_:r) x -> runP (cont ()) r (nextTab x)

nextTab :: Int -> Int
nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

tAB_LENGTH :: Int
tAB_LENGTH = 8

-- Consume and return the largest string of characters satisfying p

lexWhile :: (Char -> Bool) -> Lex a String
lexWhile p = Lex $ \cont -> P $ \rss c l loc char ->
  case rss of
    [] -> runP (cont []) [] c l loc char
    (r:rs) ->
      let
        l' = case r of
              '\n' -> l + 1
              _    -> l
        c' = case r of
              '\n' -> 1
              _    -> c + 1
       in if p r
            then runP (runL ((r:) <$> lexWhile p) cont) rs c' l' loc r
            else runP (cont []) (r:rs) c l loc char

-- | lexWhile without the return value.
lexWhile_ :: (Char -> Bool) -> Lex a ()
lexWhile_ p = do _ <- lexWhile p
                 return ()

-- An alternative scan, to which we can return if subsequent scanning
-- is unsuccessful.

alternative :: Lex a v -> Lex a (Lex a v)
alternative (Lex v) = Lex $ \cont -> P $ \r x y ->
    runP (cont (Lex $ \cont' -> P $ \_r _x _y ->
        runP (v cont') r x y)) r x y

-- The source location is the coordinates of the previous token,
-- or, while scanning a token, the start of the current token.

-- col is the current column in the source file.
-- We also need to remember between scanning tokens whether we are
-- somewhere at the beginning of the line before the first token.
-- This could be done with an extra Bool argument to the P monad,
-- but as a hack we use a col value of 0 to indicate this situation.

-- Setting col to 0 is used in two places: just after emitting a virtual
-- close brace due to layout, so that next time through we check whether
-- we also need to emit a semi-colon, and at the beginning of the file,
-- by runParser, to kick off the lexer.
-- Thus when col is zero, the true column can be taken from the loc.

checkBOL :: Lex a Bool
checkBOL = Lex $ \cont -> P $ \r x y loc ->
        if x == 0 then runP (cont True) r (srcColumn loc) y loc
            else runP (cont False) r x y loc

setBOL :: Lex a ()
setBOL = Lex $ \cont -> P $ \r _ -> runP (cont ()) r 0

-- Set the loc to the current position

startToken :: Lex a ()
startToken = Lex $ \cont -> P $ \s x y _ c stk mode ->
    let loc = SrcLoc {
        srcFilename = iParseFilename mode,
        srcLine = y,
        srcColumn = x
    } in
    runP (cont ()) s x y loc c stk mode

-- Current status with respect to the offside (layout) rule:
-- LT: we are to the left of the current indent (if any)
-- EQ: we are at the current indent (if any)
-- GT: we are to the right of the current indent, or not subject to layout

getOffside :: Lex a Ordering
getOffside = Lex $ \cont -> P $ \r x y loc ch stk ->
        runP (cont (compare x (indentOfParseState stk))) r x y loc ch stk

getSrcLocL :: Lex a SrcLoc
getSrcLocL = Lex $ \cont -> P $ \i x y l ->
        runP (cont (l { srcLine = y, srcColumn = x })) i x y l

setSrcLineL :: Int -> Lex a ()
setSrcLineL y = Lex $ \cont -> P $ \i x _ ->
        runP (cont ()) i x y

pushContextL :: LexContext -> Lex a ()
pushContextL ctxt = Lex $ \cont -> P $ \r x y loc ch (stk, exts, e, pst, cs) ->
        runP (cont ()) r x y loc ch (ctxt:stk, exts, e, pst, cs)

popContextL :: String -> Lex a ()
popContextL _ = Lex $ \cont -> P $ \r x y loc ch stk m -> case stk of
        (_:ctxt, exts, e, pst, cs) -> runP (cont ()) r x y loc ch (ctxt, exts, e, pst, cs) m
        ([], _, _, _, _)           -> Failed loc "Unexpected }"

pullCtxtFlag :: Lex a Bool
pullCtxtFlag = Lex $ \cont -> P $ \r x y loc ch (ct, exts, e, (d,c), cs) ->
        runP (cont c) r x y loc ch (ct, exts, e, (d,False), cs)


flagDo :: Lex a ()
flagDo = Lex $ \cont -> P $ \r x y loc ch (ct, exts, e, (_,c), cs) ->
        runP (cont ()) r x y loc ch (ct, exts, e, (True,c), cs)


-- Harp/Hsx

getExtContext :: Lex a (Maybe ExtContext)
getExtContext = Lex $ \cont -> P $ \r x y loc ch stk@(_, _, e, _, _) ->
        let me = case e of
              [] -> Nothing
              (c:_) -> Just c
        in runP (cont me) r x y loc ch stk

pushExtContextL :: ExtContext -> Lex a ()
pushExtContextL ec = Lex $ \cont -> P $ \r x y loc ch (s, exts, e, p, c) ->
        runP (cont ()) r x y loc ch (s, exts, ec:e, p, c)

popExtContextL :: String -> Lex a ()
popExtContextL fn = Lex $ \cont -> P $ \r x y loc ch (s,exts,e,p,c) m -> case e of
            (_:ec)   -> runP (cont ()) r x y loc ch (s,exts,ec,p,c) m
            []       -> Failed loc ("Internal error: empty tag context in " ++ fn)


-- Extension-aware lexing

getExtensionsL :: Lex a [KnownExtension]
getExtensionsL = Lex $ \cont -> P $ \r x y loc ch s m ->
        runP (cont $ iExtensions m) r x y loc ch s m

-- | Add an extension to the current configuration.
addExtensionL :: KnownExtension -> Lex a ()
addExtensionL ext = Lex $ \cont -> P $ \r x y loc ch (s, oldExts, e, p, c) m ->
        let newExts = impliesExts [ext] ++ iExtensions m
        in runP (cont ()) r x y loc ch (s, oldExts, e, p, c) (m {iExtensions = newExts})

-- | Save the current configuration of extensions.
saveExtensionsL :: Lex a ()
saveExtensionsL = Lex $ \cont -> P $ \r x y loc ch (s, oldExts, e, p, c) m ->
        runP (cont ()) r x y loc ch (s, iExtensions m:oldExts, e, p, c) m

-- | Return to the previous saved extensions configuration.
restoreExtensionsL :: Lex a ()
restoreExtensionsL = Lex $ \cont -> P $ \r x y loc ch (s,exts,e,p,c) m -> case exts of
            (_:prev) -> runP (cont ()) r x y loc ch (s,prev,e,p,c) m
            _        -> Failed loc "Internal error: empty extension stack"

-- LINE-aware lexing

ignoreLinePragmasL :: Lex a Bool
ignoreLinePragmasL = Lex $ \cont -> P $ \r x y loc c s m ->
        runP (cont $ iIgnoreLinePragmas m) r x y loc c s m

-- If we read a file name in a LINE pragma, we should update the state.
setLineFilenameL :: String -> Lex a ()
setLineFilenameL name = Lex $ \cont -> P $ \r x y loc ch s m ->
        runP (cont ()) r x y loc ch s (m {iParseFilename = name})

-- Comments

pushComment :: Comment -> Lex a ()
pushComment c = Lex $ \cont -> P $ \r x y loc ch (s, exts, e, p, cs) ->
        runP (cont ()) r x y loc ch (s, exts, e, p, c:cs)
