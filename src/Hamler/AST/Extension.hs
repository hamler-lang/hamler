{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.Extension
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, niklas.broberg@chalmers.se
-- Stability   :  transient
-- Portability :  portable
--
-- This module defines the list of recognized modular features
-- of Haskell, most often (sloppily) referred to as "extensions".
--
-- Closely mimicking the Language.Haskell.Extension module from
-- the Cabal library, this package also includes functionality for
-- "computing" languages as sets of features. Also, we make no
-- promise not to add extensions not yet recognized by Cabal.
--
-----------------------------------------------------------------------------
module Hamler.AST.Extension (
    -- * Language definitions
    Language(..),
    knownLanguages,
    classifyLanguage,
    prettyLanguage,

    -- * Extensions
    Extension(..), KnownExtension(..),
    classifyExtension,
    parseExtension, prettyExtension,

    -- * Extension groups
    ghcDefault, glasgowExts,
    knownExtensions, deprecatedExtensions,

    -- * Semantics of extensions applied to languages
    impliesExts, toExtensionList

    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<|>))
#else
import Control.Applicative ((<|>))
#endif
import Data.Array (Array, accumArray, bounds, Ix(inRange), (!))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Data

-- Copyright notice from Cabal's Language.Haskell.Extension,
-- from which we borrow plenty of features:

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

data Language =

  -- | The Haskell 98 language as defined by the Haskell 98 report.
  -- <http://haskell.org/onlinereport/>
     Haskell98

  -- | The Haskell 2010 language as defined by the Haskell 2010 report.
  -- <http://www.haskell.org/onlinereport/haskell2010>
  | Haskell2010

  -- | The minimal language resulting from disabling all recognized
  -- extensions - including ones that are part of all known language
  -- definitions e.g. MonomorphismRestriction.
  | HaskellAllDisabled

  -- | An unknown language, identified by its name.
  | UnknownLanguage String
  deriving (Show, Read, Eq, Ord, Data, Typeable)

knownLanguages :: [Language]
knownLanguages = [Haskell98, Haskell2010]

classifyLanguage :: String -> Language
classifyLanguage str = fromMaybe (UnknownLanguage str) $ lookup str langTable
  where
    langTable = [ (show lang, lang)
                | lang <- knownLanguages ]

prettyLanguage :: Language -> String
prettyLanguage (UnknownLanguage name) = name
prettyLanguage lang = show lang

-- | This represents language extensions beyond a base 'Language' definition
-- (such as 'Haskell98') that are supported by some implementations, usually
-- in some special mode.

data Extension =
  -- | Enable a known extension
    EnableExtension KnownExtension

  -- | Disable a known extension
  | DisableExtension KnownExtension

  -- | An unknown extension, identified by the name of its @LANGUAGE@
  -- pragma.
  | UnknownExtension String
  deriving (Show, Read, Eq, Ord)


data KnownExtension =

  -- | [GHC &#xa7; 7.6.3.4] Allow overlapping class instances,
  -- provided there is a unique most specific instance for each use.
    OverlappingInstances

  -- | [GHC &#xa7; 7.6.3.3] Ignore structural rules guaranteeing the
  -- termination of class instance resolution.  Termination is
  -- guaranteed by a fixed-depth recursion stack, and compilation
  -- may fail if this depth is exceeded.
  | UndecidableInstances

  -- | [GHC &#xa7; 7.6.3.4] Implies 'OverlappingInstances'.  Allow the
  -- implementation to choose an instance even when it is possible
  -- that further instantiation of types will lead to a more specific
  -- instance being applicable.
  | IncoherentInstances

  -- | [GHC &#xa7; 7.6.3.5] Allow type signatures in instances.
  | InstanceSigs

  -- | [GHC &#xa7; 7.3.8] Deprecated in GHC.  Allows recursive
  -- bindings in @do@ blocks, using the @rec@ keyword.
  | DoRec

  -- | [GHC &#xa7; 7.3.8.2] Allows recursive bindings using @mdo@, a
  -- variant of @do@, and @rec@.
  | RecursiveDo

  -- | [GHC &#xa7; 7.3.9] Provide syntax for writing list
  -- comprehensions which iterate over several lists together, like
  -- the 'zipWith' family of functions.
  | ParallelListComp

  -- | [GHC &#xa7; 7.6.1.1] Allow multiple parameters in a type class.
  | MultiParamTypeClasses

  -- | [GHC &#xa7; 7.17] Enable the dreaded monomorphism restriction.
  | MonomorphismRestriction

  -- | [GHC &#xa7; 7.6.2] Allow a specification attached to a
  -- multi-parameter type class which indicates that some parameters
  -- are entirely determined by others. The implementation will check
  -- that this property holds for the declared instances, and will use
  -- this property to reduce ambiguity in instance resolution.
  | FunctionalDependencies

  -- | [GHC &#xa7; 7.8.5] Like 'RankNTypes' but does not allow a
  -- higher-rank type to itself appear on the left of a function
  -- arrow.
  | Rank2Types

  -- | [GHC &#xa7; 7.8.5] Allow a universally-quantified type to occur on
  -- the left of a function arrow.
  | RankNTypes

  -- | [GHC &#xa7; 7.8.5] Allow data constructors to have polymorphic
  -- arguments.  Unlike 'RankNTypes', does not allow this for ordinary
  -- functions.
  | PolymorphicComponents

  -- | [GHC &#xa7; 7.4.4] Allow existentially-quantified data constructors.
  | ExistentialQuantification

  -- | [GHC &#xa7; 7.8.7] Cause a type variable in a signature, which has an
  -- explicit @forall@ quantifier, to scope over the definition of the
  -- accompanying value declaration.
  | ScopedTypeVariables

  -- | Deprecated, use 'ScopedTypeVariables' instead.
  | PatternSignatures

  -- | [GHC &#xa7; 7.8.3] Enable implicit function parameters with dynamic
  -- scope.
  | ImplicitParams

  -- | [GHC &#xa7; 7.8.2] Relax some restrictions on the form of the context
  -- of a type signature.
  | FlexibleContexts

  -- | [GHC &#xa7; 7.6.3.2] Relax some restrictions on the form of the
  -- context of an instance declaration.
  | FlexibleInstances

  -- | [GHC &#xa7; 7.4.1] Allow data type declarations with no constructors.
  | EmptyDataDecls

  -- | [GHC &#xa7; 4.10.3] Run the C preprocessor on Haskell source code.
  | CPP

  -- | [GHC &#xa7; 7.8.4] Allow an explicit kind signature giving the kind of
  -- types over which a type variable ranges.
  | KindSignatures

  -- | [GHC &#xa7; 7.6.3.1] Allow type synonyms in instance heads.
  | TypeSynonymInstances

  -- | [GHC &#xa7; 8] Enable the Foreign Function Interface.  In GHC,
  -- implements the standard Haskell 98 Foreign Function Interface
  -- Addendum, plus some GHC-specific extensions.
  | ForeignFunctionInterface

  -- | [GHC &#xa7; 7.16] Enable generic type classes, with default instances
  -- defined in terms of the algebraic structure of a type.
  | Generics

  -- | [GHC &#xa7; 7.3.11] Enable the implicit importing of the module
  -- @Prelude@.  When disabled, when desugaring certain built-in syntax
  -- into ordinary identifiers, use whatever is in scope rather than the
  -- @Prelude@ -- version.
  | ImplicitPrelude

  -- | [GHC &#xa7; 7.3.15] Enable syntax for implicitly binding local names
  -- corresponding to the field names of a record.  Puns bind specific
  -- names, unlike 'RecordWildCards'.
  | NamedFieldPuns

  -- | [GHC &#xa7; 7.3.5] Enable a form of guard which matches a pattern and
  -- binds variables.
  | PatternGuards

  -- | [GHC &#xa7; 7.5.4] Allow a type declared with @newtype@ to use
  -- @deriving@ for any class with an instance for the underlying type.
  | GeneralizedNewtypeDeriving

  -- | [GHC &#xa7; 9.6.10] Allow use of any typeclass in deriving clauses.
  | DeriveAnyClass

  -- | [Hugs &#xa7; 7.1] Enable the \"Trex\" extensible records system.
  | ExtensibleRecords

  -- | [Hugs &#xa7; 7.2] Enable type synonyms which are transparent in
  -- some definitions and opaque elsewhere, as a way of implementing
  -- abstract datatypes.
  | RestrictedTypeSynonyms

  -- | [Hugs &#xa7; 7.3] Enable an alternate syntax for string literals,
  -- with string templating.
  | HereDocuments

  -- | Binary integer literals
  | BinaryLiterals

  -- | [GHC &#xa7; 7.7] Allow data types and type synonyms which are
  -- indexed by types, i.e. ad-hoc polymorphism for types.
  | TypeFamilies

  -- | [GHC &#xa7; 7.5.2] Allow a standalone declaration which invokes the
  -- type class @deriving@ mechanism.
  | StandaloneDeriving

  -- | [GHC &#xa7; 7.3.1] Allow certain Unicode characters to stand for
  -- certain ASCII character sequences, e.g. keywords and punctuation.
  | UnicodeSyntax

  -- | [GHC &#xa7; 8.1.1] Allow the use of unboxed types as foreign types,
  -- e.g. in @foreign import@ and @foreign export@.
  | UnliftedFFITypes

  -- | [GHC &#xa7; 7.4.3] Defer validity checking of types until after
  -- expanding type synonyms, relaxing the constraints on how synonyms
  -- may be used.
  | LiberalTypeSynonyms

  -- | [GHC &#xa7; 7.4.2] Allow the name of a type constructor, type class,
  -- or type variable to be an infix operator.
  | TypeOperators

  -- | [GHC &#xa7; 7.26.5] Parallel arrays for Data Parallel Haskell.
  | ParallelArrays

  -- | [GHC &#xa7; 7.3.16] Enable syntax for implicitly binding local names
  -- corresponding to the field names of a record.  A wildcard binds
  -- all unmentioned names, unlike 'NamedFieldPuns'.
  | RecordWildCards

  -- | Deprecated, use 'NamedFieldPuns' instead.
  | RecordPuns

  -- | [GHC &#xa7; 7.3.14] Allow a record field name to be disambiguated
  -- by the type of the record it's in.
  | DisambiguateRecordFields

  -- | [GHC &#xa7; 7.6.4] Enable overloading of string literals using a
  -- type class, much like integer literals.
  | OverloadedStrings

  -- | [GHC &#xa7; 7.4.6] Enable generalized algebraic data types, in
  -- which type variables may be instantiated on a per-constructor
  -- basis. Implies GADTSyntax.
  | GADTs

{- GADTSyntax (the extension name) not yet supported by HSE

  -- | Enable GADT syntax for declaring ordinary algebraic datatypes.
  | GADTSyntax

-}

  -- | [GHC &#xa7; 7.17.2] Make pattern bindings monomorphic.
  | MonoPatBinds

  -- | [GHC &#xa7; 7.8.8] Relax the requirements on mutually-recursive
  -- polymorphic functions.
  | RelaxedPolyRec

  -- | [GHC &#xa7; 2.4.5] Allow default instantiation of polymorphic
  -- types in more situations.
  | ExtendedDefaultRules

  -- | [GHC &#xa7; 7.2.2] Enable unboxed tuples.
  | UnboxedTuples

  -- | [GHC &#xa7; 7.5.3] Enable @deriving@ for classes
  -- @Data.Typeable.Typeable@ and @Data.Generics.Data@.
  | DeriveDataTypeable

  -- | [GHC &#xa7; 7.6.1.3] Allow a class method's type to place
  -- additional constraints on a class type variable.
  | ConstrainedClassMethods

  -- | [GHC &#xa7; 7.3.18] Allow imports to be qualified by the package
  -- name the module is intended to be imported from, e.g.
  --
  -- > import "network" Network.Socket
  | PackageImports

  -- | [GHC &#xa7; 7.3.20] Allow case expressions with no alternatives.
  | EmptyCase

  -- | [GHC &#xa7; 7.8.6] Deprecated in GHC 6.12 and will be removed in
  -- GHC 7.  Allow a type variable to be instantiated at a
  -- polymorphic type.
  | ImpredicativeTypes

  -- | [GHC &#xa7; 7.3.3] Change the syntax for qualified infix
  -- operators.
  | NewQualifiedOperators

  -- | [GHC &#xa7; 7.3.12] Relax the interpretation of left operator
  -- sections to allow unary postfix operators.
  | PostfixOperators

  -- | [GHC &#xa7; 7.3.10] Enable generalized list comprehensions,
  -- supporting operations such as sorting and grouping.
  | TransformListComp

  -- | [GHC &#xa7; 7.3.6] Enable view patterns, which match a value by
  -- applying a function and matching on the result.
  | ViewPatterns

  -- | Enables the use of tuple sections, e.g. @(, True)@ desugars into
  -- @\x -> (x, True)@.
  | TupleSections

  -- | Allows GHC primops, written in C--, to be imported into a Haskell
  -- file.
  | GHCForeignImportPrim

  -- | Support for patterns of the form @n + k@, where @k@ is an
  -- integer literal.
  | NPlusKPatterns

  -- | Improve the layout rule when @if@ expressions are used in a @do@
  -- block.
  | DoAndIfThenElse

  -- | Makes much of the Haskell sugar be desugared into calls to the
  -- function with a particular name that is in scope.
  | RebindableSyntax

  -- | Make @forall@ a keyword in types, which can be used to give the
  -- generalisation explicitly.
  | ExplicitForAll

  -- | Allow contexts to be put on datatypes, e.g. the @Eq a@ in
  -- @data Eq a => Set a = NilSet | ConsSet a (Set a)@.
  | DatatypeContexts

  -- | Local (@let@ and @where@) bindings are monomorphic.
  | MonoLocalBinds

  -- | Enable @deriving@ for the @Data.Functor.Functor@ class.
  | DeriveFunctor

  -- | [GHC &#xa7; 7.5.3] Enable @deriving@ for the @Generic@ and @Generic1@ classes.
  | DeriveGeneric

  -- | Enable @deriving@ for the @Data.Traversable.Traversable@ class.
  | DeriveTraversable

  -- | Enable @deriving@ for the @Data.Foldable.Foldable@ class.
  | DeriveFoldable

  -- | Enable non-decreasing indentation for 'do' blocks.
  | NondecreasingIndentation

  -- | [GHC &#xa7; 8.1.4] Enable interruptible FFI.
  | InterruptibleFFI

  -- | [GHC &#xa7; 8.1.5] Enable the 'capi' calling convention in the
  -- foreign function interface.
  | CApiFFI

  -- | GHCJS FFI extension with convenient import patterns,
  -- asynchronous FFI and a JSVal FFI type
  | JavaScriptFFI

  -- | [GHC &#xa7; 7.3.27] Enable explicit namespaces in import/export.
  | ExplicitNamespaces

  | DataKinds

  | PolyKinds

  -- | [GHC &#xa7; 7.3.16] Enable the multi-way if-expressions
  -- extension to accept conditional expressions with multiple branches.
  | MultiWayIf

  -- | [GHC &#xa7; 7.20] Compile a module in the Trustworthy, Safe
  -- Haskell mode -- no restrictions apply but the module is marked
  -- as trusted as long as the package the module resides in is
  -- trusted.
  | Trustworthy

  -- | [GHC &#xa7; 7.6.1.4] Allow default signatures in class
  -- definitions. They apply to default methods implemented in this
  -- class.
  | DefaultSignatures


-- ConstraintKinds not generally supported by HSE. This flag makes RCategory2.hs
-- pass testing though.

  -- | [GHC &#xa7; 7.40] Allow type class/implicit parameter/equality
  -- constraints to be used as types with the special kind Constraint.
  -- Also generalise the (ctxt => ty) syntax so that any type of kind
  -- Constraint can occur before the arrow.
  | ConstraintKinds

  -- | [GHC &#xa7; 7.3.8] Enable giving names to parametrized pattern
  -- schemes
  | PatternSynonyms

  -- | [GHC &#xa7; 7.15] Allow partial specified type signatures.
  -- Note that this extension does not affect parsing. It only affects whether the
  -- program is able to be run with partial signatures.
  | PartialTypeSignatures

  -- | [GHC &#xa7; 7.15.1.2] This extension treats type variables which
  -- start with an underscore as wildcards. For example, `foo :: _x` is
  -- equivalent to `foo :: _`.
  | NamedWildCards

  | TypeFamilyDependencies

  | OverloadedLabels

  -- | Allow multiple @deriving@ clauses, each optionally qualified with a
  -- /strategy/.
  | DerivingStrategies

  | UnboxedSums

  | TypeInType

  | Strict

  | StrictData

  -- | Enable deriving instances via types of the same runtime representation.
  -- Implies 'DerivingStrategies'.
  | DerivingVia

  | QuantifiedConstraints

  | BlockArguments

  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable)

-- | Certain extensions imply other extensions, and this function
--   makes the implication explicit. This also handles deprecated
--   extensions, which imply their replacements.
--   The returned value is the transitive closure of implied
--   extensions.
{-impliesExts :: [Extension] -> [Extension]
impliesExts exts =
    let posExts = [ ke | EnableExtension  ke <- exts ]
        negExts = [ ke | DisableExtension ke <- exts ]

        implExts = impliesKnownExts posExts
     in
-}
impliesExts :: [KnownExtension] -> [KnownExtension]
impliesExts = go
  where go [] = []
        go es = let xs = concatMap implE es
                    ys = filter (not . flip elem es) xs
                 in es ++ go ys

        implE e = case e of
                    TypeFamilies        -> [KindSignatures]
                    ScopedTypeVariables -> [TypeOperators, ExplicitForAll]
                    RankNTypes          -> [Rank2Types, ExplicitForAll]
                    Rank2Types          -> [PolymorphicComponents, ExplicitForAll]
                    PolymorphicComponents   -> [ExplicitForAll]
                    LiberalTypeSynonyms -> [ExplicitForAll]
                    ExistentialQuantification -> [ExplicitForAll]
                    ImpredicativeTypes   -> [ExplicitForAll]
                    PolyKinds           -> [KindSignatures]
                    TypeFamilyDependencies -> [TypeFamilies]
                    TypeInType          -> [PolyKinds, DataKinds, KindSignatures]
                    TypeOperators       -> [ExplicitNamespaces]
                    -- Deprecations
                    RecordPuns          -> [NamedFieldPuns]
                    PatternSignatures   -> [ScopedTypeVariables]
                    DerivingVia         -> [DerivingStrategies]
                    _                   -> []

-- | The list of extensions enabled by
--   GHC's portmanteau -fglasgow-exts flag.
glasgowExts :: [Extension]
glasgowExts = map EnableExtension [
      ForeignFunctionInterface
    , UnliftedFFITypes
    , GADTs
    , ImplicitParams
    , ScopedTypeVariables
    , UnboxedTuples
    , TypeSynonymInstances
    , StandaloneDeriving
    , DeriveDataTypeable
    , FlexibleContexts
    , FlexibleInstances
    , ConstrainedClassMethods
    , MultiParamTypeClasses
    , FunctionalDependencies
    , PolymorphicComponents
    , ExistentialQuantification
    , UnicodeSyntax
    , PostfixOperators
    , PatternGuards
    , LiberalTypeSynonyms
    , RankNTypes
    , ImpredicativeTypes
    , TypeOperators
    , RecursiveDo
    , ParallelListComp
    , EmptyDataDecls
    , KindSignatures
    , GeneralizedNewtypeDeriving
    , TypeFamilies
    ]

-- Not exported, just used locally in several places.
allLangDefault :: [KnownExtension]
allLangDefault = [MonomorphismRestriction, MonoPatBinds, ImplicitPrelude]

ghcDefault :: [Extension]
ghcDefault = map EnableExtension (NondecreasingIndentation:allLangDefault)

-- | List of all known extensions, both \"yes\" and \"no\" versions.
knownExtensions :: [Extension]
knownExtensions =
  concat [ [EnableExtension x, DisableExtension x] | x <- [minBound..maxBound] ]

-- | Extensions that have been deprecated, possibly paired with another
-- extension that replaces it.
--
deprecatedExtensions :: [(Extension, Maybe Extension)]
deprecatedExtensions =
  [ (EnableExtension RecordPuns, Just (EnableExtension NamedFieldPuns))
  , (EnableExtension PatternSignatures, Just (EnableExtension ScopedTypeVariables))
  ]



-- | A clever version of read that returns an 'UnknownExtension'
--   if the string is not recognised.
classifyExtension :: String -> Extension
classifyExtension string
  = case classifyKnownExtension string of
    Just ext -> EnableExtension ext
    Nothing ->
        case string of
        'N':'o':string' ->
            case classifyKnownExtension string' of
            Just ext -> DisableExtension ext
            Nothing -> UnknownExtension string
        _ -> UnknownExtension string


classifyKnownExtension :: String -> Maybe KnownExtension
classifyKnownExtension "" = Nothing
classifyKnownExtension string@(c : _)
  | inRange (bounds knownExtensionTable) c
  = lookup string (knownExtensionTable ! c)
  | otherwise = Nothing

knownExtensionTable :: Array Char [(String, KnownExtension)]
knownExtensionTable =
  accumArray (flip (:)) [] ('A', 'Z')
    [ (head str, (str, extension))
    | extension <- [toEnum 0 ..]
    , let str = show extension ]

-- | Parse an enabled or disabled extension; returns
-- 'UnknownExtension' if the parse fails.
parseExtension :: String -> Extension
parseExtension str = fromMaybe (UnknownExtension str) $
      EnableExtension  <$> readMay str
  <|> DisableExtension <$> (readMay =<< dropNo str)
  where
    dropNo ('N':'o':rest) = Just rest
    dropNo _              = Nothing

-- | Pretty print an extension. Disabled extensions are prefixed with
-- \'No\'.
prettyExtension :: Extension -> String
prettyExtension (EnableExtension  ext) = show ext
prettyExtension (DisableExtension ext) = "No" ++ show ext
prettyExtension (UnknownExtension str) = str

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing

{-------------------------------------------
 -- Transform a 'Language', and possibly a modifying set of'Extension's, into a list
 -- of 'KnownExtension's, to be interpreted as modifying the language you get
 -- when all known extensions are disabled.
 -- Extensions are interpreted in a right-biased fashion, so the last instance
 -- of an occurence of 'EnableExtension' or 'DisableExtension' for a given
 -- 'KnownExtension' takes precedence.
 -------------------------------------------}

toExtensionList :: Language -> [Extension] -> [KnownExtension]
toExtensionList lang exts' =
    let langKes = case lang of
                    Haskell98 -> NPlusKPatterns:allLangDefault
                    Haskell2010 -> [DoAndIfThenElse
                                   , PatternGuards
                                   , ForeignFunctionInterface
                                   , EmptyDataDecls
                                   ] ++ allLangDefault
                    HaskellAllDisabled -> []
                    UnknownLanguage s ->
                        error $ "toExtensionList: Unknown language " ++ s
{-
        addExts = [ ke | EnableExtension  ke <- exts ]
        remExts = [ ke | DisableExtension ke <- exts ]
     in impliesExts $ nub $ (langKes ++ addExts) \\ remExts
-}
  in impliesExts $ nub $ go langKes exts'
    where go :: [KnownExtension] -> [Extension] -> [KnownExtension]
          go acc [] = acc
          go acc (DisableExtension x : exts) = go (filter (/= x) acc) exts
          go acc (EnableExtension  x : exts) = go (x : acc)           exts
          -- We just throw away UnknownExtensions
          go acc (_ : exts) = go acc exts
