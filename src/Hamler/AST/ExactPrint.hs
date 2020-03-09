{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.ExactPrint
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Exact-printer for Haskell abstract syntax. The input is a (semi-concrete)
-- abstract syntax tree, annotated with exact source information to enable
-- printing the tree exactly as it was parsed.
--
-----------------------------------------------------------------------------
module Hamler.AST.ExactPrint
    ( exactPrint
    , ExactP
    ) where

import Hamler.AST.Syntax
import Hamler.AST.SrcLoc
import Hamler.AST.Comments

import Control.Monad (when, liftM, ap, unless)
import qualified Control.Monad.Fail as Fail
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
#endif
import Control.Arrow ((***), (&&&))
import Prelude hiding (exp)

------------------------------------------------------
-- The EP monad and basic combinators

type Pos = (Int,Int)

pos :: (SrcInfo loc) => loc -> Pos
pos ss = (startLine ss, startColumn ss)

newtype EP x = EP (Pos -> [Comment] -> (x, Pos, [Comment], ShowS))

instance Functor EP where
  fmap = liftM

instance Applicative EP where
  pure = return
  (<*>) = ap

instance Monad EP where
  return x = EP $ \l cs -> (x, l, cs, id)

  EP m >>= k = EP $ \l0 c0 -> let
        (a, l1, c1, s1) = m l0 c0
        EP f = k a
        (b, l2, c2, s2) = f l1 c1
    in (b, l2, c2, s1 . s2)

instance Fail.MonadFail EP where
  fail = error

runEP :: EP () -> [Comment] -> String
runEP (EP f) cs = let (_,_,_,s) = f (1,1) cs in s ""

getPos :: EP Pos
getPos = EP (\l cs -> (l,l,cs,id))

setPos :: Pos -> EP ()
setPos l = EP (\_ cs -> ((),l,cs,id))

printString :: String -> EP ()
printString str =
  EP (\(l,c) cs -> let (l', c') = foldl go (l, c) str
                       go (cl, _) '\n' = (cl + 1, 1)
                       go (cl, cc) _    = (cl, cc + 1)
                   in ((), (l', c'), cs, showString str))


getComment :: EP (Maybe Comment)
getComment = EP $ \l cs ->
    let x = case cs of
             c:_ -> Just c
             _   -> Nothing
     in (x, l, cs, id)

dropComment :: EP ()
dropComment = EP $ \l cs ->
    let cs' = case cs of
               (_:cs1) -> cs1
               _       -> cs
     in ((), l, cs', id)

newLine :: EP ()
newLine = do
    (l,_) <- getPos
    printString "\n"
    setPos (l+1,1)

padUntil :: Pos -> EP ()
padUntil (l,c) = do
    (l1,c1) <- getPos
    case  {- trace (show ((l,c), (l1,c1))) -} () of
     _ {-()-} | l1 >= l && c1 <= c -> printString $ replicate (c - c1) ' '
              | l1 < l             -> newLine >> padUntil (l,c)
              | otherwise          -> return ()


mPrintComments :: Pos -> EP ()
mPrintComments p = do
    mc <- getComment
    case mc of
     Nothing -> return ()
     Just (Comment multi s str) ->
        when (pos s < p) $ do
            dropComment
            padUntil (pos s)
            printComment multi str
            setPos (srcSpanEndLine s, srcSpanEndColumn s)
            mPrintComments p

printComment :: Bool -> String -> EP ()
printComment b str
    | b         = printString $ "{-" ++ str ++ "-}"
    | otherwise = printString $ "--" ++ str

printWhitespace :: Pos -> EP ()
printWhitespace p = mPrintComments p >> padUntil p

printStringAt :: Pos -> String -> EP ()
printStringAt p str = printWhitespace p >> printString str

errorEP :: String -> EP a
errorEP = fail

------------------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST exactly as specified by the annotations on the nodes in the tree.
exactPrint :: (ExactP ast) => ast SrcSpanInfo -> [Comment] -> String
exactPrint ast = runEP (exactPC ast)

exactPC :: (ExactP ast) => ast SrcSpanInfo -> EP ()
exactPC ast = let p = pos (ann ast) in mPrintComments p >> padUntil p >> exactP ast

printSeq :: [(Pos, EP ())] -> EP ()
printSeq [] = return ()
printSeq ((p,pr):xs) = printWhitespace p >> pr >> printSeq xs

printStrs :: SrcInfo loc => [(loc, String)] -> EP ()
printStrs = printSeq . map (pos *** printString)

printPoints :: SrcSpanInfo -> [String] -> EP ()
printPoints l = printStrs . zip (srcInfoPoints l)

printInterleaved, printInterleaved' :: (ExactP ast, SrcInfo loc) => [(loc, String)] -> [ast SrcSpanInfo] -> EP ()
printInterleaved sistrs asts = printSeq $
    interleave (map (pos *** printString ) sistrs)
               (map (pos . ann &&& exactP) asts)

printInterleaved' sistrs (a:asts) = exactPC a >> printInterleaved sistrs asts
printInterleaved' _ _ = internalError "printInterleaved'"

printStreams :: [(Pos, EP ())] -> [(Pos, EP ())] -> EP ()
printStreams [] ys = printSeq ys
printStreams xs [] = printSeq xs
printStreams (x@(p1,ep1):xs) (y@(p2,ep2):ys)
    | p1 <= p2 = printWhitespace p1 >> ep1 >> printStreams xs (y:ys)
    | otherwise = printWhitespace p2 >> ep2 >> printStreams (x:xs) ys


interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y: interleave xs ys

maybeEP :: (a -> EP ()) -> Maybe a -> EP ()
maybeEP = maybe (return ())

bracketList :: (ExactP ast) => (String, String, String) -> [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
bracketList (a,b,c) poss asts = printInterleaved (pList poss (a,b,c)) asts

pList :: [a] -> (b, b, b) -> [(a, b)]
pList (p:ps) (a,b,c) = (p,a) : pList' ps (b,c)
pList _ _ = internalError "pList"
pList' :: [a] -> (b, b) -> [(a, b)]
pList' [] _ = []
pList' [p] (_,c) = [(p,c)]
pList' (p:ps) (b,c) = (p, b) : pList' ps (b,c)

parenList, squareList, curlyList, parenHashList,
  unboxedSumTypeList :: (ExactP ast) => [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
parenList = bracketList ("(",",",")")
squareList = bracketList ("[",",","]")
curlyList = bracketList ("{",",","}")
parenHashList = bracketList ("(#",",","#)")
unboxedSumTypeList = bracketList ("(#", "|", "#)")

layoutList :: (ExactP ast) => [SrcSpan] -> [ast SrcSpanInfo] -> EP ()
layoutList poss asts = printStreams
        (map (pos *** printString) $ lList poss)
        (map (pos . ann &&& exactP) asts)

lList :: [SrcSpan] -> [(SrcSpan, String)]
lList (p:ps) = (if isNullSpan p then (p,"") else (p,"{")) : lList' ps
lList _ = internalError "lList"
lList' :: [SrcSpan] -> [(SrcSpan, String)]
lList' [] = []
lList' [p] = [if isNullSpan p then (p,"") else (p,"}")]
lList' (p:ps) = (if isNullSpan p then (p,"") else (p,";")) : lList' ps

printSemi :: SrcSpan -> EP ()
printSemi p = do
  printWhitespace (pos p)
  unless (isNullSpan p) $ printString ";"


--------------------------------------------------
-- Exact printing

class Annotated ast => ExactP ast where
  exactP :: ast SrcSpanInfo -> EP ()

instance ExactP Literal where
  exactP lit = case lit of
    Char       _ _ rw -> printString ('\'':rw ++ "\'")
    String     _ _ rw -> printString ('\"':rw ++ "\"")
    Int        _ _ rw -> printString rw
    Frac       _ _ rw -> printString rw
    PrimInt    _ _ rw -> printString (rw ++ "#" )
    PrimWord   _ _ rw -> printString (rw ++ "##")
    PrimFloat  _ _ rw -> printString (rw ++ "#" )
    PrimDouble _ _ rw -> printString (rw ++ "##")
    PrimChar   _ _ rw -> printString ('\'':rw ++ "\'#" )
    PrimString _ _ rw -> printString ('\"':rw ++ "\"#" )

instance ExactP Sign where
  exactP sg = case sg of
    Signless _ -> return ()
    Negative l -> printStringAt (pos l) "-"

instance ExactP ModuleName where
  exactP (ModuleName _ str) = printString str

instance ExactP SpecialCon where
  exactP sc = case sc of
    UnitCon l   -> printPoints l ["(",")"]
    ListCon l   -> printPoints l ["[","]"]
    FunCon  l   -> case srcInfoPoints l of
                    [_,b,_] -> printStringAt (pos b) "->"
                    _ -> errorEP "ExactP: SpecialCon is given wrong number of srcInfoPoints"
    TupleCon l b n -> printPoints l $
        case b of
         Unboxed -> "(#": replicate (n-1) "," ++ ["#)"]
         _       -> "(" : replicate (n-1) "," ++ [")"]
    Cons _      -> printString ":"
    UnboxedSingleCon l -> printPoints l ["(#","#)"]
    ExprHole l -> printStringAt (pos l) "_"

isSymbolName :: Name l -> Bool
isSymbolName (Symbol _ _) = True
isSymbolName _            = False

isSymbolQName :: QName l -> Bool
isSymbolQName (UnQual _ n)         = isSymbolName n
isSymbolQName (Qual _ _ n)         = isSymbolName n
isSymbolQName (Special _ Cons{})   = True
isSymbolQName (Special _ FunCon{}) = True
isSymbolQName _                    = False

instance ExactP QName where
  exactP qn
    | isSymbolQName qn =
        case srcInfoPoints (ann qn) of
         [_,b,c] -> do
            printString "("
            printWhitespace (pos b)
            epQName qn
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: QName is given wrong number of srcInfoPoints"
    | otherwise = epQName qn

epQName :: QName SrcSpanInfo -> EP ()
epQName qn = case qn of
    Qual    _ mn n  -> exactP mn >> printString "." >> exactP n
    UnQual  _    n  -> exactP n
    Special _ sc    -> exactP sc

epInfixQName :: QName SrcSpanInfo -> EP ()
epInfixQName qn
    | isSymbolQName qn = printWhitespace (pos (ann qn)) >> epQName qn
    | otherwise =
        case srcInfoPoints (ann qn) of
         [a,b,c] -> do
            printStringAt (pos a) "`"
            printWhitespace (pos b)
            epQName qn
            printStringAt (pos c) "`"
         _ -> errorEP "ExactP: QName (epInfixName) is given wrong number of srcInfoPoints"

instance ExactP Name where
  exactP n = case n of
    Ident  _ str    -> printString str
    Symbol l str    ->
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "("
            printWhitespace (pos b)
            printString str
            printStringAt (pos c) ")"
         [] -> printString str
         _ -> errorEP "ExactP: Name is given wrong number of srcInfoPoints"

epInfixName :: Name SrcSpanInfo -> EP ()
epInfixName n
    | isSymbolName n = printWhitespace (pos (ann n)) >> exactP n
    | otherwise =
        case srcInfoPoints (ann n) of
         [a,b,c] -> do
            printStringAt (pos a) "`"
            printWhitespace (pos b)
            exactP n
            printStringAt (pos c) "`"
         _ -> errorEP "ExactP: Name (epInfixName) is given wrong number of srcInfoPoints"

instance ExactP IPName where
  exactP ipn = case ipn of
    IPDup _ str -> printString $ '?':str
    IPLin _ str -> printString $ '%':str

instance ExactP QOp where
  exactP qop = case qop of
    QVarOp _ qn -> epInfixQName qn
    QConOp _ qn -> epInfixQName qn

instance ExactP Op where
  exactP op = case op of
    VarOp _ n -> epInfixName n
    ConOp _ n -> epInfixName n

instance ExactP CName where
  exactP cn = case cn of
    VarName _ n -> exactP n
    ConName _ n -> exactP n

instance ExactP Namespace where
  exactP ns = case ns of
     NoNamespace _   -> return ()
     TypeNamespace l -> printStringAt (pos l) "type"
     PatternNamespace l -> printStringAt (pos l) "pattern"

instance ExactP ExportSpec where
  exactP espec = case espec of
     EVar _ qn    -> exactPC qn
     EAbs _ ns qn -> exactP ns >> exactPC qn
     EThingWith l wc qn cns    ->
         let names = case wc of
                        NoWildcard {} -> cns
                        EWildcard wcl n  ->
                          let (before,after) = splitAt n cns
                              wildcardName = VarName wcl (Ident wcl "..")
                          in before ++ [wildcardName] ++ after
             k = length (srcInfoPoints l)
         in exactP qn >> printInterleaved (zip (srcInfoPoints l) $ "(":replicate (k-2) "," ++ [")"]) names
     EModuleContents _ mn -> printString "module" >> exactPC mn

instance ExactP ExportSpecList where
  exactP (ExportSpecList l ess) =
    let k = length (srcInfoPoints l)
     in printInterleaved (zip (srcInfoPoints l) $ "(": replicate (k-2) "," ++ [")"]) ess

instance ExactP ImportSpecList where
  exactP (ImportSpecList l hid ispecs) = do
    let pts = srcInfoPoints l
    pts1 <- if hid then do
             let (x:pts') = pts
             printStringAt (pos x) "hiding"
             return pts'
            else return pts
    let k = length pts1
    printInterleaved (zip pts1 $ "(": replicate (k-2) "," ++ [")"]) ispecs

instance ExactP ImportSpec where
  exactP ispec = case ispec of
    IVar _ qn       -> exactPC qn
    IAbs _ ns n     -> exactP ns >> exactPC n
    IThingAll l n   -> exactP n >> printPoints l ["(","..",")"]
    IThingWith l n cns    ->
        let k = length (srcInfoPoints l)
         in exactP n >> printInterleaved (zip (srcInfoPoints l) $ "(":replicate (k-2) "," ++ [")"]) cns

instance ExactP ImportDecl where
  exactP (ImportDecl l mn qf mpkg mas mispecs) = do
    printString "import"
    case srcInfoPoints l of
     (_:pts) -> do
        pts1 <- if qf then
                 case pts of
                  x:pts' -> do
                     printStringAt (pos x) "qualified"
                     return pts'
                  _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"
                else return pts
        pts2 <- case mpkg of
                Just pkg ->
                  case pts1 of
                   x:pts' -> do
                      printStringAt (pos x) $ show pkg
                      return pts'
                   _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"
                _ -> return pts1
        exactPC mn
        _ <- case mas of
                Just as ->
                 case pts2 of
                  x:pts' -> do
                     printStringAt (pos x) "as"
                     exactPC as
                     return pts'
                  _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"
                _ -> return pts2
        case mispecs of
         Nothing -> return ()
         Just ispecs -> exactPC ispecs
     _ -> errorEP "ExactP: ImportDecl is given too few srcInfoPoints"

instance ExactP Module where
  exactP mdl = case mdl of
    Module l mmh oss ids decls -> do
        let (oPts, pts) = splitAt (max (length oss + 1) 2) (srcInfoPoints l)
        layoutList oPts oss
        maybeEP exactPC mmh
        printStreams (map (pos *** printString) $ lList pts)
                     (map (pos . ann &&& exactPC) ids ++ map (pos . ann &&& exactPC) (sepFunBinds decls))

instance ExactP ModuleHead where
  exactP (ModuleHead l mn mwt mess) =
    case srcInfoPoints l of
     [a,b] -> do
        printStringAt (pos a) "module"
        exactPC mn
        maybeEP exactPC mwt
        maybeEP exactPC mess
        printStringAt (pos b) "where"
     _ -> errorEP "ExactP: ModuleHead is given wrong number of srcInfoPoints"

instance ExactP ModulePragma where
  exactP op = case op of
    LanguagePragma   l ns       ->
        let pts = srcInfoPoints l
            k = length ns - 1 -- number of commas
            m = length pts - k - 2 -- number of virtual semis, likely 0
         in printInterleaved (zip pts ("{-# LANGUAGE":replicate k "," ++ replicate m "" ++ ["#-}"])) ns
    OptionsPragma    l mt str   ->
        let k = length (srcInfoPoints l)
            -- We strip out a leading space in the lexer unless the pragma
            -- starts with a newline.
            addSpace xs@('\n':_) = xs
            addSpace xs = ' ':xs
            opstr = "{-# OPTIONS" ++ case mt of { Just t -> "_" ++ show t ; _ -> "" } ++ addSpace str
         in printPoints l $ opstr : replicate (k-2) "" ++ ["#-}"]
    AnnModulePragma  l ann'      ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "{-# ANN"
            exactPC ann'
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: ModulePragma: AnnPragma is given wrong number of srcInfoPoints"

instance ExactP WarningText where
    exactP (DeprText l str) = printPoints l ["{-# DEPRECATED", str, "#-}"]
    exactP (WarnText l str) = printPoints l ["{-# WARNING",    str, "#-}"]

instance ExactP Assoc where
  exactP a = case a of
    AssocNone  _ -> printString "infix"
    AssocLeft  _ -> printString "infixl"
    AssocRight _ -> printString "infixr"

instance ExactP DataOrNew where
  exactP (DataType _) = printString "data"
  exactP (NewType  _) = printString "newtype"

instance ExactP TypeEqn where
  exactP (TypeEqn l t1 t2) =
    case srcInfoPoints l of
      [a] -> do
         exactPC t1
         printStringAt (pos a) "="
         exactPC t2
      _ -> errorEP "ExactP: TypeEqn is given wrong number of srcInfoPoints"

instance ExactP InjectivityInfo where
  exactP (InjectivityInfo l to from) =
    case srcInfoPoints l of
      a:b:_ -> do
        printStringAt (pos a) "|"
        exactPC to
        printStringAt (pos b) "->"
        mapM_ exactPC from
      _ -> errorEP "ExactP: InjectivityInfo given wrong number of srcInfoPoints"

instance ExactP ResultSig where
  exactP (KindSig l k) =
    case srcInfoPoints l of
      a:_ -> do
        printStringAt (pos a) "::"
        exactPC k
      _ -> errorEP "ExactP: ResultSig given wrong number of srcInfoPoints"
  exactP (TyVarSig l tv) =
    case srcInfoPoints l of
      a:_ -> do
        printStringAt (pos a) "="
        exactPC tv
      _ -> errorEP "ExactP: ResultSig given wrong number of srcInfoPoints"


instance ExactP Decl where
  exactP decl = case decl of
    TypeDecl     l dh t      ->
        case srcInfoPoints l of
         [a,b] -> do
            printStringAt (pos a) "type"
            exactPC dh
            printStringAt (pos b) "="
            exactPC t
         _ -> errorEP "ExactP: Decl: TypeDecl is given wrong number of srcInfoPoints"
    TypeFamDecl  l dh mk mi   ->
        case srcInfoPoints l of
         a:b:_ -> do
            printStringAt (pos a) "type"
            printStringAt (pos b) "family"
            exactPC dh
            maybeEP exactPC mk
            maybeEP exactPC mi
         _ -> errorEP "ExactP: Decl: TypeFamDecl is given wrong number of srcInfoPoints"
    ClosedTypeFamDecl  l dh mk mi eqns ->
        case srcInfoPoints l of
         a:b:c:_ -> do
            printStringAt (pos a) "type"
            printStringAt (pos b) "family"
            exactPC dh
            maybeEP exactPC mk
            maybeEP exactPC mi
            printStringAt (pos c) "where"
            mapM_ exactP eqns
         _ -> errorEP "ExactP: Decl: ClosedTypeFamDecl is given wrong number of srcInfoPoints"
    DataDecl     l dn mctxt dh constrs mder -> do
        exactP dn
        maybeEP exactPC mctxt
        exactPC dh
        -- the next line works for empty data types since the srcInfoPoints will be empty then
        printInterleaved (zip (srcInfoPoints l) ("=": repeat "|")) constrs
        mapM_ exactPC mder
    GDataDecl    l dn mctxt dh mk gds mder -> do
        let pts = srcInfoPoints l
        exactP dn
        maybeEP exactPC mctxt
        exactPC dh
        pts1 <- case mk of
                Nothing -> return pts
                Just kd -> case pts of
                            p:pts' -> do
                               printStringAt (pos p) "::"
                               exactPC kd
                               return pts'
                            _ -> errorEP "ExactP: Decl: GDataDecl is given too few srcInfoPoints"
        case pts1 of
         x:pts' -> do
            printStringAt (pos x) "where"
            layoutList pts' gds
            mapM_ exactPC mder
         _ -> errorEP "ExactP: Decl: GDataDecl is given too few srcInfoPoints"
    DataFamDecl  l mctxt dh mk -> do
        printString "data"
        maybeEP exactPC mctxt
        exactPC dh
        maybeEP (\kd -> printStringAt (pos (head (srcInfoPoints l))) "::" >> exactPC kd) mk
    TypeInsDecl  l t1 t2 ->
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "type"
            printStringAt (pos b) "instance"
            exactPC t1
            printStringAt (pos c) "="
            exactPC t2
         _ -> errorEP "ExactP: Decl: TypeInsDecl is given wrong number of srcInfoPoints"
    DataInsDecl  l dn t constrs mder    ->
        case srcInfoPoints l of
         p:pts -> do
            exactP dn
            printStringAt (pos p) "instance"
            exactPC t
            printInterleaved (zip pts ("=": repeat "|")) constrs
            mapM_ exactPC mder
         _ -> errorEP "ExactP: Decl: DataInsDecl is given too few srcInfoPoints"
    GDataInsDecl l dn t mk gds mder     ->
        case srcInfoPoints l of
         p:pts -> do
            exactP dn
            printStringAt (pos p) "instance"
            exactPC t
            pts1 <- case mk of
                    Nothing -> return pts
                    Just kd -> case pts of
                                p':pts' -> do
                                    printStringAt (pos p') "::"
                                    exactPC kd
                                    return pts'
                                _ -> errorEP "ExactP: Decl: GDataInsDecl is given too few srcInfoPoints"
            case pts1 of
             x:pts' -> do
                printStringAt (pos x) "where"
                layoutList pts' gds
                mapM_ exactPC mder
             _ -> errorEP "ExactP: Decl: GDataInsDecl is given too few srcInfoPoints"
         _ -> errorEP "ExactP: Decl: GDataInsDecl is given too few srcInfoPoints"
    ClassDecl    l mctxt dh fds mcds    ->
        case srcInfoPoints l of
         _:pts -> do
            printString "class"
            maybeEP exactPC mctxt
            exactPC dh
            _ <- case fds of
                    [] -> return pts
                    _  -> do
                      let (pts1, pts2) = splitAt (length fds) pts
                      printInterleaved (zip pts1 ("|":repeat ",")) fds
                      return pts2
            maybeEP (\cds ->
                case pts of
                 p:pts' -> do
                    printStringAt (pos p) "where"
                    layoutList pts' $ sepClassFunBinds cds
                 _ -> errorEP "ExactP: Decl: ClassDecl is given too few srcInfoPoints"
                ) mcds
         _ -> errorEP "ExactP: Decl: ClassDecl is given too few srcInfoPoints"
    InstDecl     l movlp ih mids        ->
        case srcInfoPoints l of
         _:pts -> do
            printString "instance"
            maybeEP exactPC movlp
            exactPC ih
            maybeEP (\ids -> do
                let (p:pts') = pts
                printStringAt (pos p) "where"
                layoutList pts' $ sepInstFunBinds ids
                ) mids
         _ -> errorEP "ExactP: Decl: InstDecl is given too few srcInfoPoints"
    DerivDecl    l mds movlp ih             ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "deriving"
            maybeEP exactPC mds
            printStringAt (pos b) "instance"
            maybeEP exactPC movlp
            exactPC ih
         _ -> errorEP "ExactP: Decl: DerivDecl is given wrong number of srcInfoPoints"
    InfixDecl    l assoc mprec ops      -> do
        let pts = srcInfoPoints l
        exactP assoc
        pts1 <- case mprec of
                Nothing -> return pts
                Just prec ->
                    case pts of
                     p:pts' -> do
                        printStringAt (pos p) (show prec)
                        return pts'
                     _ -> errorEP "ExactP: Decl: InfixDecl is given too few srcInfoPoints"
        printInterleaved' (zip pts1 (repeat ",")) ops
    DefaultDecl  l ts   ->
        case srcInfoPoints l of
         _:pts -> do
            printString "default"
            printInterleaved (zip (init pts) ("(":repeat ",")) ts
            printStringAt (pos (last pts)) ")"
         _ -> errorEP "ExactP: Decl: DefaultDecl is given too few srcInfoPoints"
    TypeSig      l ns t -> do
        let pts = srcInfoPoints l
        printInterleaved' (zip pts (replicate (length pts - 1) "," ++ ["::"])) ns
        exactPC t
    PatSynSig l ns dh c1 _ c2 t -> do
        let (pat:pts) = srcInfoPoints l
        printStringAt (pos pat) "pattern"
        printInterleaved' (zip pts (replicate (length ns - 1) "," ++ ["::"])) ns
        case dh of
          Nothing -> return ()
          Just tvs ->
            -- (length ns - 1) commas + 1 for "::"
            case drop (length ns) pts of
              (a:b:_) -> do
                    printStringAt (pos a) "forall"
                    mapM_ exactPC tvs
                    printStringAt (pos b) "."
              _ -> errorEP ("ExactP: Decl: PatSynSig: Forall: is given too few srcInfoPoints" ++ show pts ++ show (drop (length ns + 1) pts))
        maybeEP exactPC c1
        maybeEP exactPC c2
        exactPC t
    FunBind      _ ms   -> mapM_ exactPC ms
    PatBind      l p rhs mbs -> do
        let pts = srcInfoPoints l
        exactP p
        exactPC rhs
        maybeEP (\bs -> printStringAt (pos (head pts)) "where" >> exactPC bs) mbs
    PatSyn l lhs rhs dir ->
      case srcInfoPoints l of
        [pat,sepPos] -> do
          let sep = case dir of
                      ImplicitBidirectional     -> "="
                      ExplicitBidirectional _ _ -> "<-"
                      Unidirectional            -> "<-"
          printStringAt (pos pat) "pattern"
          exactPC lhs
          printStringAt (pos sepPos) sep
          exactPC rhs
          case dir of
            ExplicitBidirectional bl ds -> do
              case srcInfoPoints bl of
                (w:pts) -> do
                  printStringAt (pos w) "where"
                  layoutList pts ds
                _ -> errorEP "ExactP: Decl: PaySyn: ExplicitBidirectional is given too few srcInfoPoints"
            _ -> return ()
        _ -> errorEP "ExactP: Decl: PatSyn is given too few srcInfoPoints"
    ForImp       l cc mstr n t   ->
        case srcInfoPoints l of
         _:b:pts -> do
            printString "foreign"
            printStringAt (pos b) "import"
            exactPC cc
            pts1 <- case mstr of
                      Nothing -> return pts
                      Just str -> case pts of
                                   x:pts' -> do
                                      printStringAt (pos x) (show str)
                                      return pts'
                                   _ -> errorEP "ExactP: Decl: ForImp is given too few srcInfoPoints"
            case pts1 of
             y:_ -> do
                exactPC n
                printStringAt (pos y) "::"
                exactPC t
             _ -> errorEP "ExactP: Decl: ForImp is given too few srcInfoPoints"
         _ -> errorEP "ExactP: Decl: ForImp is given too few srcInfoPoints"
    ForExp       l cc mstr n t      ->
        case srcInfoPoints l of
         _:b:pts -> do
            printString "foreign"
            printStringAt (pos b) "export"
            exactPC cc
            pts1 <- case mstr of
                      Nothing -> return pts
                      Just str -> case pts of
                                   x:pts' -> do
                                      printStringAt (pos x) (show str)
                                      return pts'
                                   _ -> errorEP "ExactP: Decl: ForExp is given too few srcInfoPoints"
            case pts1 of
             y:_ -> do
                exactPC n
                printStringAt (pos y) "::"
                exactPC t
             _ -> errorEP "ExactP: Decl: ForExp is given too few srcInfoPoints"
         _ -> errorEP "ExactP: Decl: ForExp is given too few srcInfoPoints"
    RulePragmaDecl   l rs   ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "{-# RULES"
            mapM_ exactPC rs
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: RulePragmaDecl is given too few srcInfoPoints"
    DeprPragmaDecl   l nstrs ->
        case srcInfoPoints l of
         _:pts -> do
            printString "{-# DEPRECATED"
            printWarndeprs (map pos (init pts)) nstrs
            printStringAt (pos (last pts)) "#-}"
         _ -> errorEP "ExactP: Decl: DeprPragmaDecl is given too few srcInfoPoints"
    WarnPragmaDecl   l nstrs ->
        case srcInfoPoints l of
         _:pts -> do
            printString "{-# WARNING"
            printWarndeprs (map pos (init pts)) nstrs
            printStringAt (pos (last pts)) "#-}"
         _ -> errorEP "ExactP: Decl: WarnPragmaDecl is given too few srcInfoPoints"
    InlineSig        l inl mact qn    ->
        case srcInfoPoints l of
         [_,b] -> do
            printString $ if inl then "{-# INLINE" else "{-# NOINLINE"
            maybeEP exactPC mact
            exactPC qn
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: InlineSig is given wrong number of srcInfoPoints"
    InlineConlikeSig l mact qn    ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "{-# INLINE CONLIKE"
            maybeEP exactPC mact
            exactPC qn
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: InlineConlikeSig is given wrong number of srcInfoPoints"
    SpecSig          l mact qn ts ->
        case srcInfoPoints l of
         _:pts -> do
            printString "{-# SPECIALISE"
            maybeEP exactPC mact
            exactPC qn
            printInterleaved (zip pts ("::" : replicate (length pts - 2) "," ++ ["#-}"])) ts
         _ -> errorEP "ExactP: Decl: SpecSig is given too few srcInfoPoints"
    SpecInlineSig    l b mact qn ts ->
        case srcInfoPoints l of
         _:pts -> do
            printString $ "{-# SPECIALISE " ++ if b then "INLINE" else "NOINLINE"
            maybeEP exactPC mact
            exactPC qn
            printInterleaved (zip pts ("::" : replicate (length pts - 2) "," ++ ["#-}"])) ts
         _ -> errorEP "ExactP: Decl: SpecInlineSig is given too few srcInfoPoints"
    InstSig          l ih     ->
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "{-# SPECIALISE"
            printStringAt (pos b) "instance"
            exactPC ih
            printStringAt (pos c) "#-}"
         _ -> errorEP "ExactP: Decl: InstSig is given wrong number of srcInfoPoints"
    AnnPragma       l ann'       ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "{-# ANN"
            exactPC ann'
            printStringAt (pos b) "#-}"
         _ -> errorEP "ExactP: Decl: AnnPragma is given wrong number of srcInfoPoints"
    MinimalPragma       l b      ->
        case srcInfoPoints l of
         [_,b'] -> do
            printString "{-# MINIMAL"
            maybeEP exactPC b
            printStringAt (pos b') "#-}"
         _ -> errorEP "ExactP: Decl: MinimalPragma is given wrong number of srcInfoPoints"
    CompletePragma l cls opt_ts ->
      case srcInfoPoints l of
        (t:rs) -> do
          let (cls_s, rs') = splitAt (length cls - 1) rs
          printStringAt (pos t)"{-# COMPLETE"
          printInterleaved' (zip cls_s (repeat ",")) cls
          case (rs', opt_ts) of
             ((opt_dcolon: end:_), Just tc) -> do
                printStringAt (pos opt_dcolon) "::"
                exactPC tc
                printStringAt (pos end) "#-}"
             ([end], Nothing) -> printStringAt (pos end) "#-}"
             _ -> errorEP "ExactP: Decl: CompletePragma is given wrong number of srcInfoPoints"
        _ -> errorEP "ExactP: Decl: CompletePragma is given wrong number of srcInfoPoints"

instance ExactP Annotation where
    exactP ann' = case ann' of
        Ann     _ n e   -> do
            exactP n
            exactPC e
        TypeAnn _ n e   -> do
            printString "type"
            exactPC n
            exactPC e
        ModuleAnn _ e   -> do
            printString "module"
            exactPC e

instance ExactP BooleanFormula where
    exactP b' = case b' of
        VarFormula _ n -> exactPC n
        AndFormula l bs ->
         let pts = srcInfoPoints l
         in printStreams (zip (map pos pts) (repeat $ printString ",")) (map (pos . ann &&& exactPC) bs)
        OrFormula l bs   ->
         let pts = srcInfoPoints l
         in printStreams (zip (map pos pts) (repeat $ printString "|")) (map (pos . ann &&& exactPC) bs)
        ParenFormula l b   ->
            case srcInfoPoints l of
                [a'',b''] -> printStringAt (pos a'') "(" >> exactPC b >> printStringAt (pos b'') ")"
                _ -> errorEP "ExactP: BooleanFormula: ParenFormula is given wrong number of srcInfoPoints"

printWarndeprs :: [Pos] -> [([Name SrcSpanInfo], String)] -> EP ()
printWarndeprs _ [] = return ()
printWarndeprs ps' ((ns',str'):nsts') = printWd ps' ns' str' nsts'
  where printWd :: [Pos] -> [Name SrcSpanInfo] -> String -> [([Name SrcSpanInfo], String)] -> EP ()
        printWd (p:ps) []  str nsts = printStringAt p (show str) >> printWarndeprs ps nsts
        printWd ps     [n] str nsts = exactPC n >> printWd ps [] str nsts
        printWd (p:ps) (n:ns) str nsts = exactPC n >> printStringAt p "," >> printWd ps ns str nsts
        printWd _ _ _ _ = internalError "printWd"


sepFunBinds :: [Decl SrcSpanInfo] -> [Decl SrcSpanInfo]
sepFunBinds [] = []
sepFunBinds (FunBind _ ms:ds) = map (\m -> FunBind (ann m) [m]) ms ++ sepFunBinds ds
sepFunBinds (d:ds) = d : sepFunBinds ds

sepClassFunBinds :: [ClassDecl SrcSpanInfo] -> [ClassDecl SrcSpanInfo]
sepClassFunBinds [] = []
sepClassFunBinds (ClsDecl _ (FunBind _ ms):ds) = map (\m -> ClsDecl (ann m) $ FunBind (ann m) [m]) ms ++ sepClassFunBinds ds
sepClassFunBinds (d:ds) = d : sepClassFunBinds ds

sepInstFunBinds :: [InstDecl SrcSpanInfo] -> [InstDecl SrcSpanInfo]
sepInstFunBinds [] = []
sepInstFunBinds (InsDecl _ (FunBind _ ms):ds) = map (\m -> InsDecl (ann m) $ FunBind (ann m) [m]) ms ++ sepInstFunBinds ds
sepInstFunBinds (d:ds) = d : sepInstFunBinds ds

instance ExactP DeclHead where
  exactP dh' = case dh' of
    DHead _ n           -> exactP n
    DHInfix _ tva n     -> exactP tva >> epInfixName n
    DHParen l dh        ->
        case srcInfoPoints l of
         [_,b] -> printString "(" >> exactPC dh >> printStringAt (pos b) ")"
         _ -> errorEP "ExactP: DeclHead: DeclParen is given wrong number of srcInfoPoints"
    DHApp   _ dh t      -> exactP dh >> exactPC t

instance ExactP InstRule where
  exactP ih' = case ih' of
    IRule l mtvs mctxt qn    -> do
        let pts = srcInfoPoints l
        _ <- case mtvs of
                Nothing -> return pts
                Just tvs ->
                    case pts of
                     [a,b] -> do
                        printStringAt (pos a) "forall"
                        mapM_ exactPC tvs
                        printStringAt (pos b) "."
                        return pts
                     _ -> errorEP "ExactP: InstRule: IRule is given too few srcInfoPoints"
        maybeEP exactPC mctxt
        exactPC qn
    IParen l ih        ->
        case srcInfoPoints l of
         [a,b] -> printStringAt (pos a) "(" >> exactPC ih >> printStringAt (pos b) ")"
         _ -> errorEP "ExactP: InstRule: IParen is given wrong number of srcInfoPoints"

instance ExactP InstHead where
   exactP doih' = case doih' of
    IHCon _ qn      -> exactPC qn
    IHInfix _ ta qn -> exactPC ta >> epInfixQName qn
    IHParen l doih  ->
        case srcInfoPoints l of
         [a,b] -> printStringAt (pos a) "(" >> exactPC doih >> printStringAt (pos b) ")"
         _ -> errorEP "ExactP: OrInstHead: IHParen is given wrong number of srcInfoPoints"
    IHApp _ doih t  -> exactPC doih >> exactPC t

instance ExactP TyVarBind where
  exactP (KindedVar   l n k) =
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "("
            exactPC n
            printStringAt (pos b) "::"
            exactPC k
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: TyVarBind: KindedVar is given wrong number of srcInfoPoints"
  exactP (UnkindedVar l n) =
        case srcInfoPoints l of
                 [a,_,c] -> do
                    printStringAt (pos a) "("
                    exactPC n
                    printStringAt (pos c) ")"
                 [] -> exactPC n
                 _ -> errorEP "ExactP: TyVarBind: UnkindedVar is given wrong number of srcInfoPoints"

instance ExactP Type where
  exactP t' = case t' of
    TyForall l mtvs mctxt t -> do
        let pts = srcInfoPoints l
        _ <- case mtvs of
                Nothing -> return pts
                Just tvs ->
                    case pts of
                     _:b:pts' -> do
                        printString "forall"
                        mapM_ exactPC tvs
                        printStringAt (pos b) "."
                        return pts'
                     _ -> errorEP "ExactP: Type: TyForall is given too few srcInfoPoints"
        maybeEP exactPC mctxt
        exactPC t
    TyStar  _ -> printString "*"
    TyFun   l t1 t2 ->
        case srcInfoPoints l of
         [a] -> do
            exactP t1
            printStringAt (pos a) "->"
            exactPC t2
         _ -> errorEP "ExactP: Type: TyFun is given wrong number of srcInfoPoints"
    TyTuple l bx ts ->
        case bx of
          Boxed   -> parenList (srcInfoPoints l) ts
          Unboxed -> parenHashList (srcInfoPoints l) ts
    TyUnboxedSum l es ->
      unboxedSumTypeList (srcInfoPoints l) es
    TyList  l t     ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "["
            exactPC t
            printStringAt (pos b) "]"
         _ -> errorEP "ExactP: Type: TyList is given wrong number of srcInfoPoints"
    TyApp   _ t1 t2 -> exactP t1 >> exactPC t2
    TyVar   _ n     -> exactP n
    TyCon   _ qn    -> exactP qn
    TyParen l t     ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "("
            exactPC t
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Type: TyParen is given wrong number of srcInfoPoints"
    TyInfix _ t1 qn t2 -> exactP t1 >> exactP qn >> exactPC t2
    TyKind  l t kd ->
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "("
            exactPC t
            printStringAt (pos b) "::"
            exactPC kd
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: Type: TyKind is given wrong number of srcInfoPoints"
    TyPromoted _ p -> exactPC p
    TyEquals l t0 t1 -> case srcInfoPoints l of
                          a:_ -> exactPC t0 >> printStringAt (pos a) "~" >> exactPC t1
                          _   -> errorEP "ExactP: Type: TyEquals is given wrong number of srcInfoPoints"

    TyWildCard _ mn      -> printString "_" >> maybeEP exactPC mn

instance ExactP MaybePromotedName where
  exactP (PromotedName l qn)  = case srcInfoPoints l of
    [a] -> printStringAt (pos a) "'" >> epInfixQName qn
    _ -> errorEP "ExactP: MaybePromotedName: PromotedName given wrong number of args"
  exactP (UnpromotedName _ qn) = epInfixQName qn

instance ExactP Promoted where
  exactP (PromotedInteger _ _ rw) = printString rw
  exactP (PromotedString _ _ rw)  = printString ('\"':rw ++ "\"")
  exactP (PromotedCon l True qn)  = case srcInfoPoints l of
    [a] -> printStringAt (pos a) "'" >> epQName qn
    _ -> errorEP "ExactP: Promoted: PromotedCon is given wrong number of srcInfoPoints"
  exactP (PromotedCon _ False qn) = epQName qn
  exactP (PromotedList l b pl) =
    let o | b = "'[" | otherwise = "["
        e = "]"
        pts = srcInfoPoints l
    in printInterleaved (zip pts (o: replicate (length pts - 2) "," ++ [e])) pl
  exactP (PromotedTuple l pl) =
    let o = "'("
        e = ")"
        pts = srcInfoPoints l
    in printInterleaved (zip pts (o: replicate (length pts - 2) "," ++ [e])) pl
  exactP (PromotedUnit l) = case srcInfoPoints l of
    [_,b] -> do
        printString "("
        printStringAt (pos b) ")"
    _ -> errorEP "ExactP: Promoted: PromotedUnit is given wrong number of srcInfoPoints"


instance ExactP Context where
  exactP ctxt = do
    printContext ctxt
    printStringAt (pos . last . srcInfoPoints . ann $ ctxt) "=>"

printContext :: Context SrcSpanInfo -> EP ()
printContext ctxt = do
    let l = ann ctxt
        pts = init $ srcInfoPoints l
    case ctxt of
     CxSingle _ asst -> exactP asst
     CxEmpty _ ->
        case pts of
         [a,b] -> do
            printStringAt (pos a) "("
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Context: CxEmpty is given wrong number of srcInfoPoints"
     CxTuple _ assts -> parenList pts assts


instance ExactP Asst where
  exactP asst = case asst of
    TypeA _ t -> exactP t
    IParam l ipn t    ->
        case srcInfoPoints l of
         [a] -> do
            exactP ipn
            printStringAt (pos a) "::"
            exactPC t
         _ -> errorEP "ExactP: Asst: IParam is given wrong number of srcInfoPoints"
    ParenA l asst' ->
        case take 2 $ srcInfoPoints l of
         [a,b] -> do
            printStringAt (pos a) "("
            exactPC asst'
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Asst: ParenA is given wrong number of srcInfoPoints"

instance ExactP Deriving where
  exactP (Deriving l mds ihs) =
    case srcInfoPoints l of
     _:pts -> do
        printString "deriving"
        maybeEP exactPC mds
        case pts of
         [] -> exactPC $ head ihs
         _  -> parenList pts ihs
     _ -> errorEP "ExactP: Deriving is given too few srcInfoPoints"

instance ExactP DerivStrategy where
  exactP (DerivStock _) =
    printString "stock"
  exactP (DerivAnyclass _) =
    printString "anyclass"
  exactP (DerivNewtype _) =
    printString "newtype"
  exactP (DerivVia _ ty) = do
    printString "via"
    exactP ty

instance ExactP ClassDecl where
  exactP cdecl = case cdecl of
    ClsDecl    _ d -> exactP d
    ClsDataFam l mctxt dh mk ->
        case srcInfoPoints l of
         _:pts -> do
            printString "data"
            maybeEP exactPC mctxt
            exactPC dh
            maybeEP (\kd -> printStringAt (pos (head pts)) "::" >> exactPC kd) mk
         _ -> errorEP "ExactP: ClassDecl: ClsDataFam is given too few srcInfoPoints"
    ClsTyFam   l dh mk mi ->
        case srcInfoPoints l of
         _:_ -> do
            printString "type"
            exactPC dh
            maybeEP exactPC mk
            maybeEP exactPC mi
         _ -> errorEP "ExactP: ClassDecl: ClsTyFam is given too few srcInfoPoints"
    ClsTyDef   l t1 ->
        case srcInfoPoints l of
         _:b:_ -> do -- 3 sourceInfoPoints implies parsed as "type instance"
            printString "type"
            printStringAt (pos b) "instance"
            exactPC t1
         _:_ -> do
            printString "type"
            exactPC t1
         _ -> errorEP "ExactP: ClassDecl: ClsTyDef is given too few srcInfoPoints"
    ClsDefSig  l n t    ->
        case srcInfoPoints l of
         _:b:_ -> do
            printString "default"
            exactPC n
            printStringAt (pos b) "::"
            exactPC t
         _ -> errorEP "ExactP: ClassDecl: ClsDefSig is given too few srcInfoPoints"


instance ExactP InstDecl where
  exactP idecl = case idecl of
    InsDecl   _ d -> exactP d
    InsType   l t1 t2 ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "type"
            exactPC t1
            printStringAt (pos b) "="
            exactPC t2
         _ -> internalError "InstDecl -> InsType"
    InsData   l dn t constrs mder -> do
        exactP dn
        exactPC t
        printInterleaved (zip (srcInfoPoints l) ("=": repeat "|")) constrs
        mapM_ exactPC mder
    InsGData  l dn t mk gds mder  -> do
        let pts = srcInfoPoints l
        exactP dn
        exactPC t
        pts1 <- case mk of
                Nothing -> return pts
                Just kd -> case pts of
                            p:pts' -> do
                                printStringAt (pos p) "::"
                                exactPC kd
                                return pts'
                            _ -> errorEP "ExactP: InstDecl: InsGData is given too few srcInfoPoints"
        case pts1 of
         x:_ -> do
            printStringAt (pos x) "where"
            mapM_ exactPC gds
            mapM_ exactPC mder
         _ -> errorEP "ExactP: InstDecl: InsGData is given too few srcInfoPoints"
--  InsInline l inl mact qn   -> do
--        case srcInfoPoints l of
--         [a,b] -> do
--            printString $ if inl then "{-# INLINE" else "{-# NOINLINE"
--            maybeEP exactPC mact
--            exactPC qn
--            printStringAt (pos b) "#-}"
--         _ -> errorEP "ExactP: InstDecl: InsInline is given wrong number of srcInfoPoints"

instance ExactP FunDep where
  exactP (FunDep l nxs nys) =
    case srcInfoPoints l of
     [a] -> do
        mapM_ exactPC nxs
        printStringAt (pos a) "->"
        mapM_ exactPC nys
     _ -> errorEP "ExactP: FunDep is given wrong number of srcInfoPoints"

instance ExactP QualConDecl where
  exactP (QualConDecl l mtvs mctxt cd) = do
        let pts = srcInfoPoints l
        _ <- case mtvs of
                Nothing -> return pts
                Just tvs ->
                    case pts of
                     _:b:pts' -> do
                        printString "forall"
                        mapM_ exactPC tvs
                        printStringAt (pos b) "."
                        return pts'
                     _ -> errorEP "ExactP: QualConDecl is given wrong number of srcInfoPoints"
        maybeEP exactPC mctxt
        exactPC cd

instance ExactP ConDecl where
  exactP cd = case cd of
    ConDecl _ n bts -> exactP n >> mapM_ exactPC bts
    InfixConDecl _ bta n btb -> exactP bta >> epInfixName n >> exactP btb
    RecDecl l n fds -> exactP n >> curlyList (srcInfoPoints l) fds

instance ExactP GadtDecl where
  exactP (GadtDecl l n _mtvs mctxt ns' t) =
    case ns' of
        Nothing ->
            case srcInfoPoints l of
                [a] -> do
                    exactP n
                    printStringAt (pos a) "::"
                    exactPC t
                _ -> errorEP "ExactP: GadtDecl is given wrong number of srcInfoPoints"
        Just ts ->
            case srcInfoPoints l of
                (a:b:c:d:rest) -> do
                    exactPC n
                    printStringAt (pos a) "::"
                    maybeEP exactPC mctxt
                    printStringAt (pos b) "{"
                    printInterleaved' (zip rest (repeat ",")) ts
                    printStringAt (pos c) "}"
                    printStringAt (pos d) "->"
                    exactPC t
                _ -> errorEP "ExactP: GadtDecl is given wrong number of srcInfoPoints"

instance ExactP Exp where
  exactP exp = case exp of
    Var _ qn        -> exactP qn
    OverloadedLabel _ qn -> printString ('#':qn)
    IPVar _ ipn     -> exactP ipn
    Con _ qn        -> exactP qn
    Lit _ lit       -> exactP lit
    InfixApp _ e1 op e2 -> exactP e1 >> exactPC op >> exactPC e2
    App _ e1 e2     -> exactP e1 >> exactPC e2
    NegApp _ e      -> printString "-" >> exactPC e
    Lambda l ps e   ->
        case srcInfoPoints l of
         [_,b] -> do
            -- TODO
            printString "fun"
            mapM_ exactPC ps
            printStringAt (pos b) "->"
            exactPC e
         _ -> errorEP "ExactP: Exp: Lambda is given wrong number of srcInfoPoints"
    Let l bs e      ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "let"
            exactPC bs
            printStringAt (pos b) "in"
            exactPC e
         _ -> errorEP "ExactP: Exp: Let is given wrong number of srcInfoPoints"
    If l ec et ee   -> -- traceShow (srcInfoPoints l) $ do
        -- First we need to sort out if there are any optional
        -- semicolons hiding among the srcInfoPoints.
        case srcInfoPoints l of
         (_:b:c:rest) -> do
            let (mpSemi1,pThen,rest2) =
                           if snd (spanSize b) == 4 -- this is "then", not a semi
                            then (Nothing, b, c:rest)
                            else (Just b, c, rest)
            case rest2 of
              (c':rest3) -> do
                let (mpSemi2,rest4) = if snd (spanSize c') == 4 -- this is "else", not a semi
                                       then (Nothing, rest2)
                                       else (Just c', rest3)
                case rest4 of
                  [pElse] -> do
                    -- real work starts here:
                    printString "if"
                    exactPC ec
                    maybeEP printSemi  mpSemi1
                    printStringAt (pos pThen) "then"
                    exactPC et
                    maybeEP printSemi mpSemi2
                    printStringAt (pos pElse) "else"
                    exactPC ee
                  [] -> errorEP "ExactP: Exp: If is given too few srcInfoPoints"
                  _  -> errorEP "ExactP: Exp: If is given too many srcInfoPoints"
              _ -> errorEP "ExactP: Exp: If is given too few srcInfoPoints"

         _ -> errorEP "ExactP: Exp: If is given too few srcInfoPoints"
    MultiIf l alts ->
        case srcInfoPoints l of
          _:pts -> do
            printString "if"
            layoutList pts (map GuardedAlt alts)
          _ -> internalError "Exp -> MultiIf"
    Case l e alts   ->
        case srcInfoPoints l of
         _:b:pts -> do
            printString "case"
            exactPC e
            printStringAt (pos b) "of"
            layoutList pts alts
         _ -> errorEP "ExactP: Exp: Case is given too few srcInfoPoints"
    Do l stmts      ->
        case srcInfoPoints l of
         _:pts -> do
            printString "do"
            layoutList pts stmts
         _ -> errorEP "ExactP: Exp: Do is given too few srcInfoPoints"
    MDo l stmts     ->
        case srcInfoPoints l of
         _:pts -> do
            printString "mdo"
            layoutList pts stmts
         _ -> errorEP "ExactP: Exp: Mdo is given wrong number of srcInfoPoints"
    Tuple l bx es   ->
        case bx of
          Boxed   -> parenList (srcInfoPoints l) es
          Unboxed -> parenHashList (srcInfoPoints l) es
    UnboxedSum l b a es -> do
        unboxedSumEP l b a es
    TupleSection l bx mexps -> do
        let pts = srcInfoPoints l
            (o, e) = case bx of Boxed -> ("(", ")"); Unboxed -> ("(#", "#)")
        printSeq $ interleave (zip (map pos $ init pts) (map printString (o: repeat ",")) ++ [(pos $ last pts, printString e)])
                              (map (maybe (0,0) (pos . ann) &&& maybeEP exactPC) mexps)
    List l es               -> squareList (srcInfoPoints l) es
    Paren l p               -> parenList (srcInfoPoints l) [p]
    LeftSection l e qop     ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "("
            exactPC e
            exactPC qop
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Exp: LeftSection is given wrong number of srcInfoPoints"
    RightSection l qop e    ->
        case srcInfoPoints l of
         [_,b] -> do
            printString "("
            exactPC qop
            exactPC e
            printStringAt (pos b) ")"
         _ -> errorEP "ExactP: Exp: RightSection is given wrong number of srcInfoPoints"
    RecConstr l qn fups     -> do
        let pts = srcInfoPoints l
        exactP qn
        curlyList pts fups
    RecUpdate l e fups      -> do
        let pts = srcInfoPoints l
        exactP e
        curlyList pts fups
    EnumFrom l e            ->
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "["
            exactPC e
            printStringAt (pos b) ".."
            printStringAt (pos c) "]"
         _ -> errorEP "ExactP: Exp: EnumFrom is given wrong number of srcInfoPoints"
    EnumFromTo l e1 e2      ->
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "["
            exactPC e1
            printStringAt (pos b) ".."
            exactPC e2
            printStringAt (pos c) "]"
         _ -> errorEP "ExactP: Exp: EnumFromTo is given wrong number of srcInfoPoints"
    EnumFromThen l e1 e2    ->
        case srcInfoPoints l of
         [_,b,c,d] -> do
            printString "["
            exactPC e1
            printStringAt (pos b) ","
            exactPC e2
            printStringAt (pos c) ".."
            printStringAt (pos d) "]"
         _ -> errorEP "ExactP: Exp: EnumFromThen is given wrong number of srcInfoPoints"
    EnumFromThenTo l e1 e2 e3   ->
        case srcInfoPoints l of
         [_,b,c,d] -> do
            printString "["
            exactPC e1
            printStringAt (pos b) ","
            exactPC e2
            printStringAt (pos c) ".."
            exactPC e3
            printStringAt (pos d) "]"
         _ -> errorEP "ExactP: Exp: EnumFromToThen is given wrong number of srcInfoPoints"
    ListComp l e qss            ->
        case srcInfoPoints l of
         _:pts -> do
            printString "["
            exactPC e
            bracketList ("|",",","]") pts qss
         _ -> errorEP "ExactP: Exp: ListComp is given too few srcInfoPoints"
    ParComp  l e qsss           ->
        case srcInfoPoints l of
         _:pts -> do
            let (strs, qss) = unzip $ pairUp qsss
            printString "["
            exactPC e
            printInterleaved (zip pts (strs ++ ["]"])) qss
         _ -> errorEP "ExactP: Exp: ParComp is given wrong number of srcInfoPoints"
      where pairUp [] = []
            pairUp ((a:as):xs) = ("|", a) : zip (repeat ",") as ++ pairUp xs
            pairUp _ = internalError "Exp -> ParComp -> pairUp"

    ExpTypeSig l e t    ->
        case srcInfoPoints l of
         [a] -> do
            exactP e
            printStringAt (pos a) "::"
            exactPC t
         _ -> errorEP "ExactP: Exp: ExpTypeSig is given wrong number of srcInfoPoints"
    CorePragma l      str e         ->
        case srcInfoPoints l of
         [_,b] -> do
            printString $ "{-# CORE " ++ show str
            printStringAt (pos b) "#-}"
            exactPC e
         _ -> errorEP "ExactP: Exp: CorePragma is given wrong number of srcInfoPoints"
    SCCPragma  l      str e         ->
        case srcInfoPoints l of
         [_,b] -> do
            printString $ "{-# SCC " ++ show str
            printStringAt (pos b) "#-}"
            exactPC e
         _ -> errorEP "ExactP: Exp: SCCPragma is given wrong number of srcInfoPoints"
    GenPragma  l      str (i1,i2) (i3,i4) e -> do
        printStrs $ zip (srcInfoPoints l) ["{-# GENERATED", show str, show i1, ":", show i2, "-", show i3, ":", show i4, "#-}"]
        exactPC e

unboxedSumEP :: ExactP e => SrcSpanInfo -> Int -> Int -> e SrcSpanInfo -> EP ()
unboxedSumEP l b _a es = do
        let (opt:pts) = srcInfoPoints l
            (o, e) = ("(#", "#)")
            bars = zip (map pos (init pts)) (map printString (repeat "|"))
            open = (pos opt, printString o)
            close = (pos (last pts), printString e)
            fs = take b bars
            as = drop b bars
        printSeq $ open : fs ++ [((0, 0), exactPC es)] ++ as ++ [close]

instance ExactP FieldUpdate where
  exactP fup = case fup of
    FieldUpdate l qn e  ->
      case srcInfoPoints l of
       [a] -> do
          exactP qn
          printStringAt (pos a) "="
          exactPC e
       _ -> errorEP "ExactP: FieldUpdate is given wrong number of srcInfoPoints"
    FieldPun _ n    -> exactP n
    FieldWildcard _ -> printString ".."

instance ExactP Stmt where
  exactP stmt = case stmt of
    Generator l p e ->
      case srcInfoPoints l of
       [a] -> do
          exactP p
          printStringAt (pos a) "<-"
          exactPC e
       _ -> errorEP "ExactP: Stmt: Generator is given wrong number of srcInfoPoints"
    Qualifier _ e -> exactP e
    LetStmt _ bds   -> do
      printString "let"
      exactPC bds
    RecStmt l ss    ->
      case srcInfoPoints l of
       _:pts -> do
          printString "rec"
          layoutList pts ss
       _ -> errorEP "ExactP: Stmt: RecStmt is given too few srcInfoPoints"

instance ExactP QualStmt where
  exactP qstmt = case qstmt of
    QualStmt     _ stmt -> exactP stmt

instance ExactP Alt where
  exactP (Alt l p galts mbs) = do
    exactP p
    exactPC (GuardedAlts galts)
    maybeEP (\bs -> printStringAt (pos (head (srcInfoPoints l))) "where" >> exactPC bs) mbs

instance ExactP Match where
  exactP (Match l n ps rhs mbinds) = do
    let pts = srcInfoPoints l
        len = length pts
        pars = len `div` 2
        (oPars,cParsWh) = splitAt pars pts
        (cPars,_) = splitAt pars cParsWh    -- _§ is either singleton or empty
    printStrs (zip oPars (repeat "("))
    exactPC n
    printStreams (zip (map pos cPars) (repeat $ printString ")")) (map (pos . ann &&& exactPC) ps)
    exactPC rhs
    maybeEP (\bds -> printStringAt (pos (head pts)) "where" >> exactPC bds) mbinds
  exactP (InfixMatch l a n bs rhs mbinds) = do
    let pts = srcInfoPoints l
        len = length pts
        pars = len `div` 2
        (oPars,cParsWh) = splitAt pars pts
        (cPars,whPt) = splitAt pars cParsWh    -- whPt is either singleton or empty
    printStrs (zip oPars (repeat "("))
    exactPC a
    epInfixName n
    printInterleaved' (zip cPars (repeat ")")) bs
    exactPC rhs
    maybeEP (\bds -> printStringAt (pos (head whPt)) "where" >> exactPC bds) mbinds

instance ExactP Rhs where
  exactP (UnGuardedRhs _ e) = printString "=" >> exactPC e
  exactP (GuardedRhss  _ grhss) = mapM_ exactPC grhss

instance ExactP GuardedRhs where
  exactP (GuardedRhs l ss e) =
    case srcInfoPoints l of
     _:pts -> do
        printString "|"
        printInterleaved' (zip (init pts) (repeat ",") ++ [(last pts, "=")]) ss
        exactPC e
     _ -> errorEP "ExactP: GuardedRhs is given wrong number of srcInfoPoints"

newtype GuardedAlts l = GuardedAlts (Rhs l)
    deriving (Functor, Show)
instance Annotated GuardedAlts where
    amap f (GuardedAlts v) = GuardedAlts (amap f v)
    ann (GuardedAlts v) = ann v

newtype GuardedAlt l = GuardedAlt (GuardedRhs l)
    deriving (Functor, Show)
instance Annotated GuardedAlt where
    amap f (GuardedAlt v) = GuardedAlt (amap f v)
    ann (GuardedAlt v) = ann v

instance ExactP GuardedAlts where
  exactP (GuardedAlts (UnGuardedRhs _ e)) = printString "->" >> exactPC e
  exactP (GuardedAlts (GuardedRhss  _ grhss)) = mapM_ (exactPC . GuardedAlt) grhss

instance ExactP GuardedAlt where
  exactP (GuardedAlt (GuardedRhs l ss e)) =
    case srcInfoPoints l of
     _:pts -> do
        printString "|"
        printInterleaved' (zip (init pts) (repeat ",") ++ [(last pts, "->")]) ss
        exactPC e
     _ -> errorEP "ExactP: GuardedAlt is given wrong number of srcInfoPoints"

instance ExactP Pat where
  exactP pat = case pat of
    PVar l n    -> exactPC (fmap (const l) n)
    PLit _ sg lit -> exactPC sg >> exactPC lit
    PNPlusK l n k   ->
        case srcInfoPoints l of
         [a,b] -> do
            exactP n
            printStringAt (pos a) "+"
            printStringAt (pos b) (show k)
         _ -> errorEP "ExactP: Pat: PNPlusK is given wrong number of srcInfoPoints"
    PInfixApp _ pa qn pb -> exactP pa >> epInfixQName qn >> exactPC pb
    PApp _ qn ps    -> exactP qn >> mapM_ exactPC ps
    PTuple l bx ps ->
        case bx of
          Boxed   -> parenList (srcInfoPoints l) ps
          Unboxed -> parenHashList (srcInfoPoints l) ps
    PUnboxedSum l before after e ->
      unboxedSumEP l before after e
    PList l ps  -> squareList (srcInfoPoints l) ps
    PParen l p  -> parenList (srcInfoPoints l) [p]
    PRec l qn pfs   -> exactP qn >> curlyList (srcInfoPoints l) pfs
    PAsPat l n p    ->
        case srcInfoPoints l of
         [a] -> do
            exactP n
            printStringAt (pos a) "@"
            exactPC p
         _ -> errorEP "ExactP: Pat: PAsPat is given wrong number of srcInfoPoints"
    PWildCard _ -> printString "_"
    PIrrPat _ p -> printString "~" >> exactPC p
    PatTypeSig l p t ->
        case srcInfoPoints l of
         [a] -> do
            exactP p
            printStringAt (pos a) "::"
            exactPC t
         _ -> errorEP "ExactP: Pat: PatTypeSig is given wrong number of srcInfoPoints"
    PViewPat l e p ->
        case srcInfoPoints l of
         [a] -> do
            exactP e
            printStringAt (pos a) "->"
            exactPC p
         _ -> errorEP "ExactP: Pat: PViewPat is given wrong number of srcInfoPoints"

instance ExactP PatField where
  exactP pf = case pf of
    PFieldPat l qn p        ->
        case srcInfoPoints l of
         [a] -> do
            exactP qn
            printStringAt (pos a) "="
            exactPC p
         _ -> errorEP "ExactP: PatField: PFieldPat is given wrong number of srcInfoPoints"
    PFieldPun _ n   -> exactP n
    PFieldWildcard _ -> printString ".."

instance ExactP Binds where
  exactP (BDecls  l ds)  = layoutList (srcInfoPoints l) (sepFunBinds ds)
  exactP (IPBinds l ips) = layoutList (srcInfoPoints l) ips

instance ExactP CallConv where
  exactP (StdCall    _) = printString "stdcall"
  exactP (CCall      _) = printString "ccall"
  exactP (CPlusPlus  _) = printString "cplusplus"
  exactP (DotNet     _) = printString "dotnet"
  exactP (Jvm        _) = printString "jvm"
  exactP (Js         _) = printString "js"
  exactP (JavaScript _) = printString "javascript"
  exactP (CApi       _) = printString "capi"

instance ExactP Rule where
  exactP (Rule l str mact mrvs e1 e2) =
    case srcInfoPoints l of
     _:pts -> do
        printString (show str)
        maybeEP exactP mact
        pts1 <- case mrvs of
                Nothing -> return pts
                Just rvs ->
                    case pts of
                     a':b:pts' -> do
                        printStringAt (pos a') "forall"
                        mapM_ exactPC rvs
                        printStringAt (pos b) "."
                        return pts'
                     _ -> errorEP "ExactP: Rule is given too few srcInfoPoints"
        case pts1 of
         [x] -> do
            exactPC e1
            printStringAt (pos x) "="
            exactPC e2
         _ -> errorEP "ExactP: Rule is given wrong number of srcInfoPoints"
     _ -> errorEP "ExactP: Rule is given too few srcInfoPoints"

instance ExactP RuleVar where
  exactP (TypedRuleVar l n t) =
        case srcInfoPoints l of
         [_,b,c] -> do
            printString "("
            exactPC n
            printStringAt (pos b) "::"
            exactPC t
            printStringAt (pos c) ")"
         _ -> errorEP "ExactP: RuleVar: TypedRuleVar is given wrong number of srcInfoPoints"
  exactP (RuleVar _ n) = exactP n

instance ExactP Overlap where
  exactP (NoOverlap _) =
    printString "{-# NO_OVERLAP #-}"
  exactP (Overlap _) =
    printString "{-# OVERLAP #-}"
  exactP (Overlaps _) =
    printString "{-# OVERLAPS #-}"
  exactP (Overlapping _) =
    printString "{-# OVERLAPPING #-}"
  exactP (Overlappable _) =
    printString "{-# OVERLAPPABLE #-}"
  exactP (Incoherent _) =
    printString "{-# INCOHERENT #-}"

instance ExactP Activation where
  exactP (ActiveFrom   l i) =
    printPoints l ["[", show i, "]"]
  exactP (ActiveUntil  l i) =
    printPoints l ["[", "~", show i, "]"]

instance ExactP FieldDecl where
  exactP (FieldDecl l ns bt) = do
    let pts = srcInfoPoints l
    printInterleaved' (zip (init pts) (repeat ",") ++ [(last pts, "::")]) ns
    exactPC bt

instance ExactP IPBind where
  exactP (IPBind l ipn e) =
    case srcInfoPoints l of
     [a] -> do
        exactP ipn
        printStringAt (pos a) "="
        exactPC e
     _ -> errorEP "ExactP: IPBind is given wrong number of srcInfoPoints"

-- Hopefully, this will never fire.
-- If it does, hopefully by that time https://github.com/sol/rewrite-with-location
-- will be implemented.
-- If not, then removing all calls to internalError should give a better
-- idea where the error comes from.
-- So far, it's necessary to eliminate non-exhaustive patterns warnings.
-- We don't want to turn them off, as we want unhandled AST nodes to be
-- reported.
internalError :: String -> a
internalError loc' = error $ unlines
    [ "haskell-src-exts: ExactPrint: internal error (non-exhaustive pattern)"
    , "Location: " ++ loc'
    , "This is either caused by supplying incorrect location information or by"
    , "a bug in haskell-src-exts. If this happens on an unmodified AST obtained"
    , "by the haskell-src-exts Parser it is a bug, please it report it at"
    , "https://github.com/haskell-suite/haskell-src-exts"]
