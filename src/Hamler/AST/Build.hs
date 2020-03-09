-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.Build
-- Copyright   :  (c) The GHC Team, 1997-2000,
--                (c) Niklas Broberg 2004
--                (c) EMQ Technologies Co., Ltd. 2020
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains combinators to use when building
-- Haskell source trees programmatically, as opposed to
-- parsing them from a string. The contents here are quite
-- experimental and will likely receive a lot of attention
-- when the rest has stabilised.
--
-----------------------------------------------------------------------------

module Hamler.AST.Build (

    -- * Syntax building functions
    name,       -- :: String -> Name ()
    sym,        -- :: String -> Name ()
    var,        -- :: Name () -> Exp ()
    op,         -- :: Name () -> QOp
    qvar,       -- :: Module -> Name () -> Exp ()
    pvar,       -- :: Name () -> Pat ()
    app,        -- :: Exp () -> Exp () -> Exp ()
    infixApp,   -- :: Exp () -> QOp -> Exp () -> Exp ()
    appFun,     -- :: Exp () -> [Exp] -> Exp ()
    pApp,       -- :: Name () -> [Pat] -> Pat ()
    tuple,      -- :: [Exp] -> Exp ()
    pTuple,     -- :: [Pat] -> Pat ()
    varTuple,   -- :: [Name] -> Exp ()
    pvarTuple,  -- :: [Name] -> Pat ()
    function,   -- :: String -> Exp ()
    strE,       -- :: String -> Exp ()
    charE,      -- :: Char -> Exp ()
    intE,       -- :: Integer -> Exp ()
    strP,       -- :: String -> Pat ()
    charP,      -- :: Char -> Pat ()
    intP,       -- :: Integer -> Pat ()
    doE,        -- :: [Stmt] -> Exp ()
    lamE,       -- :: SrcLoc -> [Pat] -> Exp () -> Exp ()
    letE,       -- :: [Decl] -> Exp () -> Exp ()
    caseE,      -- :: Exp () -> [Alt] -> Exp ()
    alt,        -- :: SrcLoc -> Pat () -> Exp () -> Alt
    altGW,      -- :: SrcLoc -> Pat () -> [Stmt] -> Exp () -> Binds -> Alt
    listE,      -- :: [Exp] -> Exp ()
    eList,      -- :: Exp ()
    peList,     -- :: Pat ()
    paren,      -- :: Exp () -> Exp ()
    pParen,     -- :: Pat () -> Pat ()
    qualStmt,   -- :: Exp () -> Stmt
    genStmt,    -- :: SrcLoc -> Pat () -> Exp () -> Stmt
    letStmt,    -- :: [Decl] -> Stmt
    binds,      -- :: [Decl] -> Binds
    noBinds,    -- :: Binds
    wildcard,   -- :: Pat ()
    genNames,   -- :: String -> Int -> [Name]

    -- * More advanced building
    sfun,           -- :: SrcLoc -> Name () -> [Name] -> Rhs -> Binds -> Decl ()
    simpleFun,      -- :: SrcLoc -> Name () -> Name () -> Exp () -> Decl ()
    patBind,        -- :: SrcLoc -> Pat () -> Exp () -> Decl ()
    patBindWhere,   -- :: SrcLoc -> Pat () -> Exp () -> [Decl] -> Decl ()
    nameBind,       -- :: SrcLoc -> Name () -> Exp () -> Decl ()
    metaFunction,   -- :: String -> [Exp] -> Exp ()
    metaConPat      -- :: String -> [Pat] -> Pat ()
  ) where

import Hamler.AST.Syntax

-----------------------------------------------------------------------------
-- Help functions for Abstract syntax

-- | An identifier with the given string as its name.
--   The string should be a valid Haskell identifier.
name :: String -> Name ()
name = Ident ()

-- | A symbol identifier. The string should be a valid
--   Haskell symbol identifier.
sym :: String -> Name ()
sym = Symbol ()

-- | A local variable as expression.
var :: Name () -> Exp ()
var = Var () . UnQual ()

-- | Use the given identifier as an operator.
op :: Name () -> QOp ()
op = QVarOp () . UnQual ()

-- | A qualified variable as expression.
qvar :: ModuleName () -> Name () -> Exp ()
qvar m n = Var () $ Qual () m n

-- | A pattern variable.
pvar :: Name () -> Pat ()
pvar = PVar ()

-- | Application of expressions by juxtaposition.
app :: Exp () -> Exp () -> Exp ()
app = App ()

-- | Apply an operator infix.
infixApp :: Exp () -> QOp () -> Exp () -> Exp ()
infixApp = InfixApp ()

-- | Apply a function to a list of arguments.
appFun :: Exp () -> [Exp ()] -> Exp ()
appFun f [] = f
appFun f (a:as) = appFun (app f a) as

-- | A constructor pattern, with argument patterns.
pApp :: Name () -> [Pat ()] -> Pat ()
pApp n ps = PApp () (UnQual () n) ps

-- | A tuple expression.
tuple :: [Exp ()] -> Exp ()
tuple = Tuple () Boxed

-- | A tuple pattern.
pTuple :: [Pat ()] -> Pat ()
pTuple = PTuple () Boxed

-- | A tuple expression consisting of variables only.
varTuple :: [Name ()] -> Exp ()
varTuple ns = tuple $ map var ns

-- | A tuple pattern consisting of variables only.
pvarTuple :: [Name ()] -> Pat ()
pvarTuple ns = pTuple $ map pvar ns

-- | A function with a given name.
function :: String -> Exp ()
function = var . Ident ()

-- | A literal string expression.
strE :: String -> Exp ()
strE s = Lit () (String () s s)

-- | A literal character expression.
charE :: Char -> Exp ()
charE c = Lit () (Char () c [c])

-- | A literal integer expression.
intE :: Integer -> Exp ()
intE n = Lit () (Int () n (show n))

-- | A literal string pattern.
strP :: String -> Pat ()
strP s = PLit () (Signless ()) (String () s s)

-- | A literal character pattern.
charP :: Char -> Pat ()
charP x = PLit () (Signless ()) (Char () x [x])

-- | A literal integer pattern.
intP :: Integer -> Pat ()
intP x = PLit ()
          (if x >= 0 then Signless () else Negative ())
          (Int () (abs x) (show x))

-- | A do block formed by the given statements.
--   The last statement in the list should be
--   a 'Qualifier' expression.
doE :: [Stmt ()] -> Exp ()
doE = Do ()

-- | Lambda abstraction, given a list of argument
--   patterns and an expression body.
lamE :: [Pat ()] -> Exp () -> Exp ()
lamE = Lambda ()

-- | A @let@ ... @in@ block.
letE :: [Decl ()] -> Exp () -> Exp ()
letE ds e = Let () (binds ds) e

-- | A @case@ expression.
caseE :: Exp () -> [Alt ()] -> Exp ()
caseE = Case ()

-- | An unguarded alternative in a @case@ expression.
alt :: Pat () -> Exp () -> Alt ()
alt p e = Alt () p (unGAlt e) noBinds

-- | An alternative with a single guard in a @case@ expression.
altGW :: Pat () -> [Stmt ()] -> Exp () -> Binds () -> Alt ()
altGW p gs e w = Alt () p (gAlt gs e) (Just w)

-- | An unguarded righthand side of a @case@ alternative.
unGAlt :: Exp () -> Rhs ()
unGAlt = UnGuardedRhs ()

-- | An list of guarded righthand sides for a @case@ alternative.
gAlts :: [([Stmt ()],Exp ())] -> Rhs ()
gAlts as = GuardedRhss () $ map (\(gs,e) -> GuardedRhs () gs e) as

-- | A single guarded righthand side for a @case@ alternative.
gAlt :: [Stmt ()] -> Exp () -> Rhs ()
gAlt gs e = gAlts [(gs,e)]

-- | A list expression.
listE :: [Exp ()] -> Exp ()
listE = List ()

-- | The empty list expression.
eList :: Exp ()
eList = List () []

-- | The empty list pattern.
peList :: Pat ()
peList = PList () []

-- | Put parentheses around an expression.
paren :: Exp () -> Exp ()
paren = Paren ()

-- | Put parentheses around a pattern.
pParen :: Pat () -> Pat ()
pParen = PParen ()

-- | A qualifier expression statement.
qualStmt :: Exp () -> Stmt ()
qualStmt = Qualifier ()

-- | A generator statement: /pat/ @<-@ /exp/
genStmt :: Pat () -> Exp () -> Stmt ()
genStmt = Generator ()

-- | A @let@ binding group as a statement.
letStmt :: [Decl ()] -> Stmt ()
letStmt ds = LetStmt () $ binds ds

-- | Hoist a set of declarations to a binding group.
binds :: [Decl ()] -> Binds ()
binds = BDecls ()

-- | An empty binding group.
noBinds :: Maybe (Binds ())
noBinds = Nothing

-- | The wildcard pattern: @_@
wildcard :: Pat ()
wildcard = PWildCard ()

-- | Generate k names by appending numbers 1 through k to a given string.
genNames :: String -> Int -> [Name ()]
genNames s k = [ Ident () $ s ++ show i | i <- [1..k] ]

-------------------------------------------------------------------------------
-- Some more specialised help functions

-- | A function with a single clause
sfun :: Name () -> [Name ()] -> (Rhs ()) -> Maybe (Binds ()) -> Decl ()
sfun f pvs rhs bs = FunBind () [Match () f (map pvar pvs) rhs bs]

-- | A function with a single clause, a single argument, no guards
-- and no where declarations
simpleFun :: Name () -> Name () -> Exp () -> Decl ()
simpleFun f a e = let rhs = UnGuardedRhs () e
             in sfun f [a] rhs noBinds

-- | A pattern bind where the pattern is a variable, and where
-- there are no guards and no 'where' clause.
patBind :: Pat () -> Exp () -> Decl ()
patBind p e = let rhs = UnGuardedRhs () e
         in PatBind () p rhs noBinds

-- | A pattern bind where the pattern is a variable, and where
-- there are no guards, but with a 'where' clause.
patBindWhere :: Pat () -> Exp () -> [Decl ()] -> Decl ()
patBindWhere p e ds = let rhs = UnGuardedRhs () e
             in PatBind () p rhs (if null ds then Nothing else Just (binds ds))

-- | Bind an identifier to an expression.
nameBind :: Name () -> Exp () -> Decl ()
nameBind n e = patBind (pvar n) e

-- | Apply function of a given name to a list of arguments.
metaFunction :: String -> [Exp ()] -> Exp ()
metaFunction s' es' = mf s' (reverse es')
  where mf s []     = var $ name s
        mf s (e:es) = app (mf s es) e

-- | Apply a constructor of a given name to a list of pattern
--   arguments, forming a constructor pattern.
metaConPat :: String -> [Pat ()] -> Pat ()
metaConPat s ps = pApp (name s) ps
