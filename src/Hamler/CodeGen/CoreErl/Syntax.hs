-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErlang.Syntax
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alex Kropivny <alex.kropivny@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A suite of datatypes describing the abstract syntax of CoreErlang 1.0.3.
-- <http://www.it.uu.se/research/group/hipe/cerl/>

-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}

module Hamler.CodeGen.CoreErl.Syntax (
    -- * Modules
    EModule(..),
    -- * Declarations
    EFunDef(..),
    -- * Expressions
    EExp(..), EExps(..), EAlt(..), EGuard(..),
    EList(..), ETimeOut(..), EBitString(..), EFunction(..),
    -- * Patterns
    EPats(..), EPat(..), EAlias(..),
    -- * Literals
    ELiteral(..), EConst(..), EAtom(..),
    -- * Variables
    EVar,
    -- * Annotations
    EAnn(..),
  ) where

import Data.Data

-- | This type is used to represent variables
type EVar = String

-- | This type is used to represent atoms
data EAtom = EAtom String
 deriving (Eq,Ord,Show,Data,Typeable)

-- | This type is used to represent function names
data EFunction = EFunction (EAtom,Integer)
  deriving (Eq,Ord,Show,Data,Typeable)

-- | A CoreErlang source module.
data EModule
  = EModule EAtom [EFunction] [(EAtom,EConst)] [EFunDef]
  deriving (Eq,Ord,Show,Data,Typeable)

-- | This type is used to represent constants
data EConst = ECLit ELiteral
            | ECTuple [EConst]
            | ECList (EList EConst)
  deriving (Eq,Ord,Show,Data,Typeable)

-- | This type is used to represent lambdas
data EFunDef = EFunDef (EAnn EFunction) (EAnn EExp)
  deriving (Eq,Ord,Show,Data,Typeable)

-- | /literal/.
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used. For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data ELiteral = ELChar    Char     -- ^ character literal
              | ELString  String   -- ^ string literal
              | ELInt     Integer  -- ^ integer literal
              | ELFloat   Double   -- ^ floating point literal
              | ELAtom    EAtom    -- ^ atom literal
              | ELNil              -- ^ empty list
  deriving (Eq,Ord,Show,Data,Typeable)

-- | CoreErlang expressions.
data EExps = EExp (EAnn EExp)         -- ^ single expression
           | EExps (EAnn [EAnn EExp]) -- ^ list of expressions
  deriving (Eq,Ord,Show,Data,Typeable)

-- | CoreErlang expression.
data EExp = EVar EVar                      -- ^ variable
          | ELit ELiteral                  -- ^ literal constant
          | EFun EFunction                 -- ^ function name
          | EApp EExps [EExps]             -- ^ application
          | EModCall (EExps,EExps) [EExps] -- ^ module call
          | ELambda [EVar] EExps           -- ^ lambda expression
          | ESeq EExps EExps               -- ^ sequencing
          | ELet ([EVar],EExps) EExps      -- ^ local declaration
          | ELetRec [EFunDef] EExps        -- ^ letrec expression
          | ECase EExps [EAnn EAlt]        -- ^ @case@ /exp/ @of@ /alts/ end
          | ETuple [EExps]                 -- ^ tuple expression
          | EList (EList EExps)            -- ^ list expression
          | EBinary [EBitString EExps]     -- ^ binary expression
          | EOp EAtom [EExps]              -- ^ operator application
          | ETry EExps ([EVar],EExps) ([EVar],EExps) -- ^ try expression
          | ERec [EAnn EAlt] ETimeOut      -- ^ receive expression
          | ECatch EExps                   -- ^ catch expression
  deriving (Eq,Ord,Show,Data,Typeable)

-- | A bitstring.
data EBitString a = EBitString a [EExps]
  deriving (Eq,Ord,Show,Data,Typeable)

-- | A list of expressions
data EList a = EL [a] | ELL [a] a
  deriving (Eq,Ord,Show,Data,Typeable)

-- | An /alt/ in a @case@ expression
data EAlt = EAlt EPats EGuard EExps
  deriving (Eq,Ord,Show,Data,Typeable)

data EPats = EPat EPat    -- ^ single pattern
           | EPats [EPat] -- ^ list of patterns
  deriving (Eq,Ord,Show,Data,Typeable)

-- | A pattern, to be matched against a value.
data EPat = EPVar EVar                  -- ^ variable
          | EPLit ELiteral              -- ^ literal constant
          | EPTuple [EPat]              -- ^ tuple pattern
          | EPList (EList EPat)         -- ^ list pattern
          | EPBinary [EBitString EPat]  -- ^ list of bitstring patterns
          | EPAlias EAlias              -- ^ alias pattern
  deriving (Eq,Ord,Show,Data,Typeable)

-- | An alias, used in patterns
data EAlias = EAlias EVar EPat
  deriving (Eq,Ord,Show,Data,Typeable)

-- | A guarded alternative @when@ /exp/ @->@ /exp/.
-- The first expression will be Boolean-valued.
data EGuard = EGuard EExps
  deriving (Eq,Ord,Show,Data,Typeable)

-- | The timeout of a receive expression
data ETimeOut = ETimeOut EExps EExps
  deriving (Eq,Ord,Show,Data,Typeable)

-- | An annotation for modules, variables, ...
data EAnn a = EConstr a       -- ^ core erlang construct
            | EAnn a [EConst] -- ^ core erlang annotated construct
  deriving (Eq,Ord,Show,Data,Typeable)

