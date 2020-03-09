{
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.InternalParser
-- Copyright   :  (c) Niklas Broberg 2004-2009,
--                Original (c) Simon Marlow, Sven Panne 1997-2000
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Hamler.AST.InternalParser (
              mparseModule,
              mparseExp,
              mparsePat,
              mparseDecl,
              mparseType,
              mparseStmt,
              mparseImportDecl,
              ngparseModulePragmas,
              ngparseModuleHeadAndImports,
              ngparsePragmasAndModuleHead,
              ngparsePragmasAndModuleName
              ) where

import Hamler.AST.Syntax hiding ( Type(..), Exp(..), Asst(..), FieldUpdate(..) )
import Hamler.AST.Syntax ( Type, Exp, Asst )
import Hamler.AST.ParseMonad
import Hamler.AST.Lexer
import Hamler.AST.ParseUtils
import Hamler.AST.Fixity
import Hamler.AST.SrcLoc
import Hamler.AST.Extension

import Control.Monad ( liftM, (<=<), when )
import Control.Applicative ( (<$>) )
import Data.Maybe
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif
import Debug.Trace (trace)

}

%token
      VARID    { Loc _ (VarId _) }       -- 1
      LABELVARID { Loc _ (LabelVarId _) }
      QVARID   { Loc _ (QVarId _) }
      IDUPID   { Loc _ (IDupVarId _) }       -- duplicable implicit parameter ?x
      ILINID   { Loc _ (ILinVarId _) }       -- linear implicit parameter %x
      CONID    { Loc _ (ConId _) }
      QCONID   { Loc _ (QConId _) }
      DVARID   { Loc _ (DVarId _) }          -- VARID containing dashes
      VARSYM   { Loc _ (VarSym _) }
      CONSYM   { Loc _ (ConSym _) }
      QVARSYM  { Loc _ (QVarSym _) } -- 10
      QCONSYM  { Loc _ (QConSym _) }
      INT      { Loc _ (IntTok _) }
      RATIONAL { Loc _ (FloatTok _) }
      CHAR     { Loc _ (Character _) }
      STRING   { Loc _ (StringTok _) }

      PRIMINT     { Loc _ (IntTokHash _) }
      PRIMWORD    { Loc _ (WordTokHash _) }
      PRIMFLOAT   { Loc _ (FloatTokHash _) }
      PRIMDOUBLE  { Loc _ (DoubleTokHash _) }
      PRIMCHAR    { Loc _ (CharacterHash _) } -- 20
      PRIMSTRING  { Loc _ (StringHash _) }

-- Symbols

      '('     { Loc $$ LeftParen }
      ')'     { Loc $$ RightParen }
      '(#'    { Loc $$ LeftHashParen }
      '#)'    { Loc $$ RightHashParen }
      ';'     { Loc $$ SemiColon }
      '{'     { Loc $$ LeftCurly }
      '}'     { Loc $$ RightCurly }      -- 30
      vccurly { Loc $$ VRightCurly }                 -- a virtual close brace
      '['     { Loc $$ LeftSquare }
      ']'     { Loc $$ RightSquare }
      ','     { Loc $$ Comma }
      '_'     { Loc $$ Underscore }
      '`'     { Loc $$ BackQuote }

-- Reserved operators

      '.'     { Loc $$ Dot }
      '..'    { Loc $$ DotDot }
      ':'     { Loc $$ Colon }
      '::'    { Loc $$ DoubleColon }      -- 40
      '='     { Loc $$ Equals }
      '\\'    { Loc $$ Backslash }
      '|'     { Loc $$ Bar }
      '<-'    { Loc $$ LeftArrow }
      '->'    { Loc $$ RightArrow }
      '@'     { Loc $$ At }
      '~'     { Loc $$ Tilde }
      '=>'    { Loc $$ DoubleArrow }
      '-'     { Loc $$ Minus }
      '!'     { Loc $$ Exclamation }  -- 50
      '*'     { Loc $$ Star }

-- FFI

      'foreign'       { Loc $$ KW_Foreign }
      'export'        { Loc $$ KW_Export }    -- 80
      'stdcall'       { Loc $$ KW_StdCall }
      'ccall'         { Loc $$ KW_CCall }
      'cplusplus'     { Loc $$ KW_CPlusPlus }
      'dotnet'        { Loc $$ KW_DotNet }
      'jvm'           { Loc $$ KW_Jvm }
      'js'            { Loc $$ KW_Js }          -- 90
      'javascript'    { Loc $$ KW_JavaScript }
      'capi'          { Loc $$ KW_CApi }

-- Reserved Ids

      'as'            { Loc $$ KW_As }
      'by'            { Loc $$ KW_By }       -- transform list comprehensions
      'case'          { Loc $$ KW_Case }
      'class'         { Loc $$ KW_Class }
      'data'          { Loc $$ KW_Data }
      'default'       { Loc $$ KW_Default }
      'deriving'      { Loc $$ KW_Deriving }
      'do'            { Loc $$ KW_Do }
      'else'          { Loc $$ KW_Else }     -- 100
      'family'        { Loc $$ KW_Family }   -- indexed type families
      'forall'        { Loc $$ KW_Forall }   -- universal/existential qualification
      'fun'           { Loc $$ KW_Fun}       -- lambda
      'hiding'        { Loc $$ KW_Hiding }
      'if'            { Loc $$ KW_If }
      'import'        { Loc $$ KW_Import }
      'in'            { Loc $$ KW_In }
      'infix'         { Loc $$ KW_Infix }
      'infixl'        { Loc $$ KW_InfixL }
      'infixr'        { Loc $$ KW_InfixR }        -- 110
      'instance'      { Loc $$ KW_Instance }
      'let'           { Loc $$ KW_Let }
      'mdo'           { Loc $$ KW_MDo }
      'module'        { Loc $$ KW_Module }         -- 114
      'newtype'       { Loc $$ KW_NewType }
      'of'            { Loc $$ KW_Of }
      'then'          { Loc $$ KW_Then }
      'type'          { Loc $$ KW_Type }     -- 120
      'where'         { Loc $$ KW_Where }
      'qualified'     { Loc $$ KW_Qualified }
      'pattern'       { Loc $$ KW_Pattern }
      'stock'         { Loc $$ KW_Stock }    -- for DerivingStrategies extension
      'anyclass'      { Loc $$ KW_Anyclass } -- for DerivingStrategies extension
      'via'           { Loc $$ KW_Via }      -- for DerivingStrategies extension

-- Pragmas

      '{-# INLINE'            { Loc _ (INLINE _) }
      '{-# INLINE CONLIKE'    { Loc $$ INLINE_CONLIKE }
      '{-# SPECIALISE'        { Loc $$ SPECIALISE }
      '{-# SPECIALISE INLINE' { Loc _ (SPECIALISE_INLINE _) }
      '{-# SOURCE'            { Loc $$ SOURCE }
      '{-# RULES'             { Loc $$ RULES }
      '{-# CORE'              { Loc $$ CORE }         -- 130
      '{-# SCC'               { Loc $$ SCC }
      '{-# GENERATED'         { Loc $$ GENERATED }
      '{-# DEPRECATED'        { Loc $$ DEPRECATED }
      '{-# WARNING'           { Loc $$ WARNING }
      '{-# UNPACK'            { Loc $$ UNPACK }
      '{-# NOUNPACK'          { Loc $$ NOUNPACK }
      '{-# OPTIONS'           { Loc _ (OPTIONS _) }
      '{-# LANGUAGE'          { Loc $$ LANGUAGE }      -- 137
      '{-# ANN'               { Loc $$ ANN }
      '{-# MINIMAL'           { Loc $$ MINIMAL }
      '{-# NO_OVERLAP'        { Loc $$ NO_OVERLAP }
      '{-# OVERLAP'           { Loc $$ OVERLAP }
      '{-# OVERLAPS'          { Loc $$ OVERLAPS }
      '{-# OVERLAPPING'       { Loc $$ OVERLAPPING }
      '{-# OVERLAPPABLE'      { Loc $$ OVERLAPPABLE }
      '{-# INCOHERENT'        { Loc $$ INCOHERENT }
      '{-# COMPLETE'          { Loc $$ COMPLETE }
      '#-}'                   { Loc $$ PragmaEnd }      -- 139

-- Utility

      NEVER                   { Loc $$@SrcSpan{srcSpanStartLine= -1} _ } -- never-matching terminal of type SrcSpan

%monad { P }
%lexer { lexer } { Loc _ EOF }
%error { parseError }
%name mparseModule page
%name mparseExp trueexp
%name mparsePat pat
%name mparseDeclAux body
%name mparseType truectype
%name mparseStmt stmt
%name mparseImportDecl impdecl
%partial ngparseModulePragmas toppragmas
%partial ngparseModuleHeadAndImports moduletopimps
%partial ngparsePragmasAndModuleHead moduletophead
%partial ngparsePragmasAndModuleName moduletopname
%tokentype { Loc Token }
%expect 8
%%

-----------------------------------------------------------------------------
-- | Testing multiple modules in one file

modules :: { [Module L] }
        : toppragmas modules1         { let (os,ss,l) = $1 in map (\x -> x os ss l) $2 }

modules1 :: { [[ModulePragma L] -> [S] -> L -> Module L] }
        : module modules1             { $1 : $2 }
        | module                      { [$1] }

page :: { Module L }
      : toppragmas module                            { let (os,ss,l) = $1 in $2 os ss l }

toppragmas :: { ([ModulePragma L],[S],L) }
          : open toppragmasaux close          { let (os,ss,ml) = $2 in (os,$1:ss++[$3],$1 <^^> $3) }

toppragmasaux :: { ([ModulePragma L],[S],Maybe L) }
              : toppragma optsemis toppragmasaux      { let (os,ss,ml) = $3;
                                                             ss' = reverse $2 ++ ss;
                                                             l'  = case $2 of
                                                                    [] -> ann $1
                                                                    _  -> ann $1 <++> nIS (last $2);
                                                         in ($1 : os, ss', Just $ l'  <+?> ml) }
              | {- nothing -}                         { ([],[],Nothing) }

toppragma :: { ModulePragma L }
          : '{-# LANGUAGE' conids optsemis '#-}'   { LanguagePragma ($1 <^^> $4 <** ($1:snd $2 ++ reverse $3 ++ [$4])) (fst $2) }
          | '{-# OPTIONS' optsemis '#-}'           { let Loc l (OPTIONS (mc, s)) = $1
                                                      in OptionsPragma (l <^^> $3 <** (l:reverse $2 ++ [$3])) (readTool mc) s }
          | '{-# ANN' annotation '#-}'             { AnnModulePragma ($1 <^^> $3 <** [$1,$3]) $2 }

conids    :: { ([Name L],[S]) }
         : conids ',' conid                  { (fst $1 ++ [$3], snd $1 ++ [$2]) }
         | optsemis conid                    { ([$2],[]) }

-----------------------------------------------------------------------------
-- | Module Header

module :: { [ModulePragma L] -> [S] -> L -> Module L }
      : optmodulehead body
              { let (is,ds,ss1,inf) = $2
                 in \os ss l -> Module (l <++> inf <** (ss ++ ss1)) $1 os is ds }

optmodulehead :: { Maybe (ModuleHead L) }
      : 'module' modid maybemodwarning maybeexports 'where'   { Just $ ModuleHead ($1 <^^> $5 <** [$1,$5]) $2 $3 $4 }
      | {- empty -}                                           { Nothing }

maybemodwarning ::  { Maybe (WarningText L) }
      : '{-# DEPRECATED' STRING '#-}'         { let Loc l (StringTok (s,_)) = $2 in Just $ DeprText ($1 <^^> $3 <** [$1,l,$3]) s }
      | '{-# WARNING'    STRING '#-}'         { let Loc l (StringTok (s,_)) = $2 in Just $ WarnText ($1 <^^> $3 <** [$1,l,$3]) s }
      | {- empty -}                           { Nothing }

body :: { ([ImportDecl L],[Decl L],[S],L) }
      : '{'  bodyaux '}'                      { let (is,ds,ss) = $2 in (is,ds,$1:ss ++ [$3], $1 <^^> $3) }
      | open bodyaux close optsemis           { let (is,ds,ss) = $2 in (is,ds,$1:ss ++ [$3], $1 <^^> $3) }

bodyaux :: { ([ImportDecl L],[Decl L],[S]) }
      : optsemis impdecls semis topdecls      { (reverse (fst $2), fst $4, reverse $1 ++ snd $2 ++ reverse $3 ++ snd $4) }
      | optsemis                topdecls      { ([], fst $2, reverse $1 ++ snd $2) }
      | optsemis impdecls optsemis            { (reverse (fst $2), [], reverse $1 ++ snd $2 ++ reverse $3) }
      | optsemis                              { ([], [], reverse $1) }

semis :: { [S] }
      : optsemis ';'                          { $2 : $1 }

optsemis :: { [S] }
      : semis                                 { $1 }
      | {- empty -}                           { [] }

-----------------------------------------------------------------------------
-- | The Export List

maybeexports :: { Maybe (ExportSpecList L) }
      :  exports                              { Just $1 }
      |  {- empty -}                          { Nothing }

exports :: { ExportSpecList L }
      : '(' exportlist optcomma ')'           { ExportSpecList ($1 <^^> $4 <** ($1:reverse (snd $2) ++ $3 ++ [$4])) (reverse (fst $2)) }
      | '(' optcomma ')'                      { ExportSpecList ($1 <^^> $3 <** ($1:$2++[$3])) [] }

optcomma :: { [S] }
      : ','                                   { [$1] }
      | {- empty -}                           { [  ] }

exportlist :: { ([ExportSpec L],[S]) }
      :  exportlist ',' export                { ($3 : fst $1, $2 : snd $1) }
      |  export                               { ([$1],[])  }

export :: { ExportSpec L }
      :  qvar                                 { EVar (ann $1) $1 }
      |  'type' qcname                          {% do { checkEnabled ExplicitNamespaces;
                                                      return (EAbs (nIS $1 <++> ann $2 <** [$1, srcInfoSpan (ann $2)]) (TypeNamespace (nIS $1 <** [$1])) $2) } }
      |  qtyconorcls                          { EAbs (ann $1) (NoNamespace (ann $1)) $1 }
      |  qtyconorcls '(' ')'                  { EThingWith (ann $1 <++> nIS $3 <** [$2,$3])    (NoWildcard noSrcSpan) $1 [] }
      |  qtyconorcls '(' export_names ')'     {% mkEThingWith (ann $1 <++> nIS $4 <** ($2:reverse (snd $3) ++ [$4])) $1 (reverse $ fst $3) }
      |  'module' modid                       { EModuleContents (nIS $1 <++> ann $2 <** [$1]) $2 }
      |  'pattern' qcon                       {%  do { checkEnabled PatternSynonyms;
                                                      return $ EAbs (nIS $1 <++> (ann $2) <** [$1])
                                                                 (PatternNamespace (nIS $1)) $2 }}

export_names :: { ([Either S (CName L)],[S]) }
      :  export_names ',' cname_w_wildcard          { ($3 : fst $1, $2 : snd $1) }
      |  cname_w_wildcard                     { ([$1],[])  }

cname_w_wildcard :: { Either S (CName L) }
      :  '..'                                 { Left $1 }
      |  cname                                { Right $1 }


qcname :: { QName L }
       : qvar                                 { $1 }
       | qcon                                 { $1 }

-----------------------------------------------------------------------------
-- | Import Declarations

impdecls :: { ([ImportDecl L],[S]) }
      : impdecls semis impdecl                { ($3 : fst $1, snd $1 ++ reverse $2) }
      | impdecl                               { ([$1],[]) }

impdecl :: { ImportDecl L }
      : 'import' optqualified maybepkg modid maybeas maybeimpspec
                              { let { (mmn,ss,ml) = $5 ;
                                      l = nIS $1 <++> ann $4 <+?> ml <+?> (fmap ann) $6 <** ($1:snd $2 ++ snd $3 ++ ss)}
                                 in ImportDecl l $4 (fst $2) (fst $3) mmn $6 }

optqualified :: { (Bool,[S]) }
      : 'qualified'                           { (True,[$1]) }
      | {- empty -}                           { (False, []) }

maybepkg :: { (Maybe String,[S]) }
      : STRING                                {% do { checkEnabled PackageImports ;
                                                      let { Loc l (StringTok (s,_)) = $1 } ;
                                                      return $ (Just s,[l]) } }
      | {- empty -}                           { (Nothing,[]) }

maybeas :: { (Maybe (ModuleName L),[S],Maybe L) }
      : 'as' modid                            { (Just $2,[$1],Just (nIS $1 <++> ann $2)) }
      | {- empty -}                           { (Nothing,[],Nothing) }


maybeimpspec :: { Maybe (ImportSpecList L) }
      : impspec                               { Just $1 }
      | {- empty -}                           { Nothing }

impspec :: { ImportSpecList L }
      : opthiding '(' importlist optcomma ')' { let {(b,ml,s) = $1 ;
                                                      l = (ml <?+> ($2 <^^> $5)) <** (s ++ $2:reverse (snd $3) ++ $4 ++ [$5])}
                                                 in ImportSpecList l b (reverse (fst $3)) }
      | opthiding '(' optcomma ')'            { let {(b,ml,s) = $1 ; l = (ml <?+> ($2 <^^> $4)) <** (s ++ $2:$3 ++ [$4])}
                                                 in ImportSpecList l b [] }

opthiding :: { (Bool, Maybe L,[S]) }
      : 'hiding'                              { (True,Just (nIS $1),[$1]) }
      | {- empty -}                           { (False,Nothing,[])  }

importlist :: { ([ImportSpec L],[S]) }
      :  importlist ',' importspec            { ($3 : fst $1, $2 : snd $1) }
      |  importspec                           { ([$1],[])  }

importspec :: { ImportSpec L }
      :  var                                  { IVar (ann $1) $1 }
      |  'type' var                           {% do { checkEnabled ExplicitNamespaces;
                                                      return (IAbs (nIS $1 <++> ann $2 <** [$1, srcInfoSpan (ann $2)]) (TypeNamespace (nIS $1 <** [$1])) $2) } }
      |  'pattern' con                        {% do { checkEnabled PatternSynonyms;
                                                      return (IAbs (nIS $1 <++> ann $2 <** [$1, srcInfoSpan (ann $2)]) (PatternNamespace (nIS $1 <** [$1])) $2) } }
      |  tyconorcls                           { IAbs (ann $1) (NoNamespace (ann $1)) $1 }
      |  tyconorcls '(' '..' ')'              { IThingAll  (ann $1 <++> nIS $4 <** [$2,$3,$4]) $1 }
      |  tyconorcls '(' ')'                   { IThingWith (ann $1 <++> nIS $3 <** [$2,$3])    $1 [] }
      |  tyconorcls '(' import_names ')'            { IThingWith (ann $1 <++> nIS $4 <** ($2:reverse (snd $3) ++ [$4])) $1 (reverse (fst $3)) }

import_names :: { ([CName L],[S]) }
      :  import_names ',' cname               { ($3 : fst $1, $2 : snd $1) }
      |  cname                                { ([$1],[])  }

cname :: { CName L }
      :  var                                  { VarName (ann $1) $1 }
      |  con                                  { ConName (ann $1) $1 }

-----------------------------------------------------------------------------
-- | Fixity Declarations

fixdecl :: { Decl L }
      : infix prec ops                        { let (ops,ss,l) = $3
                                                 in InfixDecl (ann $1 <++> l <** (snd $2 ++ reverse ss)) $1 (fst $2) (reverse ops) }

prec :: { (Maybe Int, [S]) }
      : {- empty -}                           { (Nothing, []) }
      | INT                                   {% let Loc l (IntTok (i,_)) = $1 in checkPrec i >>= \i -> return (Just i, [l]) }

infix :: { Assoc L }
      : 'infix'                               { AssocNone  $ nIS $1 }
      | 'infixl'                              { AssocLeft  $ nIS $1 }
      | 'infixr'                              { AssocRight $ nIS $1 }

ops   :: { ([Op L],[S],L) }
      : ops ',' op                            { let (ops,ss,l) = $1 in ($3 : ops, $2 : ss, l <++> ann $3) }
      | op                                    { ([$1],[],ann $1) }


opt_injectivity_info :: { Maybe (InjectivityInfo L) }
       : {- empty -}                  { Nothing }
       | injectivity_info             { Just $1 }

injectivity_info :: { InjectivityInfo L }
       : '|' tyvarid '->' inj_varids
             { InjectivityInfo (nIS $1 <++> ann (last $4) <** [$1,$3]) $2 (reverse $4) }


inj_varids :: { [Name L] }
       : inj_varids tyvarid  { $2 : $1 }
       | tyvarid             { [$1]     }

-----------------------------------------------------------------------------
-- | Top-Level Declarations

topdecls :: { ([Decl L],[S]) }
      : topdecls1 optsemis            {% checkRevDecls (fst $1) >>= \ds -> return (ds, snd $1 ++ reverse $2) }

topdecls1 :: { ([Decl L],[S]) }
      : topdecls1 semis topdecl       { ($3 : fst $1, snd $1 ++ reverse $2) }
      | topdecl                       { ([$1],[]) }

topdecl :: { Decl L }
      : 'type' dtype '=' truectype
               {% do { dh <- checkSimpleType $2;
                       let {l = nIS $1 <++> ann $4 <** [$1,$3]};
                       return (TypeDecl l dh $4) } }
      | 'type' 'family' type opt_tyfam_kind_sig opt_injectivity_info where_type_family
               {% do { dh <- checkSimpleType $3;
                       let {l = nIS $1 <++> ann $3 <** [$1,$2]};
                       case $6 of {
                         Nothing    -> return (TypeFamDecl l dh $4 $5);
                         Just (x,a) -> return (ClosedTypeFamDecl (l <** [a]) dh $4 $5 x); }}}
      | 'type' 'instance' truedtype '=' truectype
               {% do { -- no checkSimpleType $4 since dtype may contain type patterns
                       checkEnabled TypeFamilies ;
                       let {l = nIS $1 <++> ann $5 <** [$1,$2,$4]};
                       return (TypeInsDecl l $3 $5) } }
      | data_or_newtype ctype constrs0 maybe_derivings
               {% do { (cs,dh) <- checkDataHeader $2;
                       let { (qds,ss,minf) = $3;
                             l = $1 <> $2 <+?> minf <+?> fmap ann (listToMaybe $4) <** ss};
                       checkDataOrNew $1 qds;
                       return (DataDecl l $1 cs dh (reverse qds) (reverse $4)) } }
      | data_or_newtype ctype optkind gadtlist maybe_derivings
               {% do { (cs,dh) <- checkDataHeader $2;
                       let { (gs,ss,minf) = $4;
                             derivs' = reverse $5;
                             l = ann $1 <+?> minf <+?> fmap ann (listToMaybe $5) <** (snd $3 ++ ss)};
                       checkDataOrNewG $1 gs;
                       case (gs, fst $3) of
                        ([], Nothing) -> return (DataDecl l $1 cs dh [] derivs')
                        _ -> checkEnabled GADTs >> return (GDataDecl l $1 cs dh (fst $3) (reverse gs) derivs') } }
      | 'data' 'family' ctype opt_datafam_kind_sig
               {% do { (cs,dh) <- checkDataHeader $3;
                       let {l = nIS $1 <++> ann $3 <+?> (fmap ann) $4 <** [$1,$2]};
                       return (DataFamDecl l cs dh $4) } }
      | data_or_newtype 'instance' truectype constrs0 maybe_derivings
               {% do { -- (cs,c,t) <- checkDataHeader $4;
                       checkEnabled TypeFamilies ;
                       let { (qds,ss,minf) = $4 ;
                             l = $1 <> $3 <+?> minf <+?> fmap ann (listToMaybe $5) <** $2:ss };
                       checkDataOrNew $1 qds;
                       return (DataInsDecl l $1 $3 (reverse qds) (reverse $5)) } }
      | data_or_newtype 'instance' truectype optkind gadtlist maybe_derivings
               {% do { -- (cs,c,t) <- checkDataHeader $4;
                       checkEnabled TypeFamilies ;
                       let {(gs,ss,minf) = $5;
                            derivs' = reverse $6;
                            l = ann $1 <+?> minf <+?> fmap ann (listToMaybe derivs') <** ($2:snd $4 ++ ss)};
                       checkDataOrNewG $1 gs;
                       return (GDataInsDecl l $1 $3 (fst $4) (reverse gs) derivs') } }
      | 'class' ctype fds optcbody
               {% do { (cs,dh) <- checkClassHeader $2;
                       let {(fds,ss1,minf1) = $3;(mcs,ss2,minf2) = $4} ;
                       let { l = nIS $1 <++> ann $2 <+?> minf1 <+?> minf2 <** ($1:ss1 ++ ss2)} ;
                       return (ClassDecl l cs dh fds mcs) } }
      | 'instance' optoverlap ctype optvaldefs
               {% do { ih <- checkInstHeader $3;
                       let {(mis,ss,minf) = $4};
                       return (InstDecl (nIS $1 <++> ann $3 <+?> minf <** ($1:ss)) $2 ih mis) } }
      | 'deriving' deriv_standalone_strategy 'instance' optoverlap ctype
               {% do { checkEnabled StandaloneDeriving ;
                       ih <- checkInstHeader $5;
                       let {l = nIS $1 <++> ann $5 <** [$1,$3]};
                       return (DerivDecl l $2 $4 ih) } }
      | 'default' '(' typelist ')'
               { DefaultDecl ($1 <^^> $4 <** ($1:$2 : snd $3 ++ [$4])) (fst $3) }

      | 'foreign' 'export' callconv fspec
               { let (s,n,t,ss) = $4 in ForExp (nIS $1 <++> ann t <** ($1:$2:ss)) $3    s n t }

      | '{-# RULES'      rules      '#-}'     { RulePragmaDecl ($1 <^^> $3 <** [$1,$3]) $ reverse $2 }
      | '{-# DEPRECATED' warndeprs  '#-}'     { DeprPragmaDecl ($1 <^^> $3 <** ($1:snd $2++[$3])) $ reverse (fst $2) }
      | '{-# WARNING'    warndeprs  '#-}'     { WarnPragmaDecl ($1 <^^> $3 <** ($1:snd $2++[$3])) $ reverse (fst $2) }
      | '{-# ANN'        annotation '#-}'     { AnnPragma      ($1 <^^> $3 <** [$1,$3]) $2 }
      | '{-# COMPLETE' con_list opt_tyconsig '#-}'
          { let com = maybe [] ((:[]) . fst) $3; ts = fmap snd $3 in
(CompletePragma ($1 <^^> $4 <** ([$1] ++ fst $2 ++ com ++ [$4])) (snd $2) ts) }
      | decl          { $1 }

-- Family result/return kind signatures

opt_datafam_kind_sig :: { Maybe (ResultSig L) }
       :               { Nothing     }
       | '::' kind     { (Just $ KindSig (nIS $1 <++> ann $2 <** [$1]) $2) }

opt_tyfam_kind_sig :: { Maybe (ResultSig L) }
       :              { Nothing       }
       | '::' kind    { (Just $ KindSig  (nIS $1 <++> ann $2 <** [$1]) $2) }
       | '='  ktyvar  { (Just $ TyVarSig (nIS $1 <++> ann $2 <** [$1]) $2) }

opt_at_kind_inj_sig :: { (Maybe (ResultSig L), Maybe (InjectivityInfo L))}
       :            { (Nothing, Nothing) }
       | '::' kind  { (Just (KindSig (nIS $1 <++> ann $2 <** [$1]) $2), Nothing) }
       | '='  ktyvar injectivity_info
               { (Just (TyVarSig (nIS $1 <++> ann $2 <** [$1]) $2), Just $3) }

opt_at_kind_inj_sig2 :: { (Maybe (ResultSig L), Maybe (S, Type L), Maybe (InjectivityInfo L))}
       :            { (Nothing, Nothing, Nothing) }
       | '::' kind  { (Just (KindSig (nIS $1 <++> ann $2 <** [$1]) $2), Nothing, Nothing) }
       | '='  truectype opt_injectivity_info { (Nothing, Just ($1, $2), $3) }

optoverlap :: { Maybe (Overlap L) }
 : '{-# OVERLAP'      '#-}'    { Just (Overlap (nIS $1)) }
 | '{-# OVERLAPS'     '#-}'    { Just (Overlaps (nIS $1)) }
 | '{-# OVERLAPPING'  '#-}'    { Just (Overlapping (nIS $1)) }
 | '{-# OVERLAPPABLE' '#-}'    { Just (Overlappable (nIS $1)) }
 | '{-# INCOHERENT' '#-}'    { Just (Incoherent (nIS $1)) }
 | '{-# NO_OVERLAP' '#-}'    { Just (NoOverlap (nIS $1))  }
 | {- empty -}               { Nothing }

-- Parsing the body of a closed type family, partially stolen from the source of GHC.
where_type_family :: { Maybe ([TypeEqn L], S) }
        : {- empty -}                  { Nothing }
        | 'where' ty_fam_inst_eqn_list { Just ($2, $1) }

ty_fam_inst_eqn_list :: { [TypeEqn L] }
        : '{'  ty_fam_inst_eqns '}'     { $2 }
        | open ty_fam_inst_eqns close   { $2 }

ty_fam_inst_eqns :: { [TypeEqn L] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn   { $1 ++ [$3] }
        | ty_fam_inst_eqns ';'                   { $1 }
        | ty_fam_inst_eqn                        { [$1] }

ty_fam_inst_eqn :: { TypeEqn L }
        : truectype '=' truectype
                {% do { checkEnabled TypeFamilies ;
                        return (TypeEqn (ann $1 <++> ann $3 <** [$2]) $1 $3) } }

data_or_newtype :: { DataOrNew L }
      : 'data'    { DataType $ nIS $1 }
      | 'newtype' { NewType  $ nIS $1 }

typelist :: { ([Type L],[S]) }
      : types                         {% do { ts <- mapM checkType (fst $1);
                                              return $ (reverse ts, reverse (snd $1)) } }
      | truetype                      { ([$1],[]) }
      | {- empty -}                   { ([],[]) }

decls :: { ([Decl L],[S]) }
      : optsemis decls1 optsemis      {% checkRevDecls (fst $2) >>= \ds -> return (ds, reverse $1 ++ snd $2 ++ reverse $3) }
      | optsemis                      { ([],reverse $1) }

decls1 :: { ([Decl L],[S]) }
      : decls1 semis decl             { ($3 : fst $1, snd $1 ++ reverse $2) }
      | decl                          { ([$1],[]) }

decl :: { Decl L }
      : signdecl                      { $1 }
      | fixdecl                       { $1 }
      | valdef                        { $1 }
      | pat_syn                       { $1 }
      | pattern_synonym_sig           { $1 }

decllist :: { Binds L }
      : '{'  decls '}'                { BDecls ($1 <^^> $3 <** ($1:snd $2++[$3])) (fst $2) }
      | open decls close              { let l' = if null (fst $2) then nIS $3 else (ann . last $ fst $2)
                                         in BDecls (nIS $1 <++> l' <** ($1:snd $2++[$3])) (fst $2) }

signdecl :: { Decl L }
      : signdecl0                     { $1 }
      | specinldecl                   { $1 }

signdecl0 :: { Decl L }
      : exp0b '::' truectype                           {% do { v <- checkSigVar $1;
                                                               return $ TypeSig ($1 <> $3 <** [$2]) [v] $3 } }
      | exp0b ',' vars '::' truectype                  {% do { v <- checkSigVar $1;
                                                               let {(vs,ss,_) = $3 ; l = $1 <> $5 <** ($2 : reverse ss ++ [$4]) } ;
                                                               return $ TypeSig l (v : reverse vs) $5 } }

specinldecl :: { Decl L }
      : '{-# INLINE' activation qvar '#-}'             { let Loc l (INLINE s) = $1 in InlineSig (l <^^> $4 <** [l,$4]) s $2 $3 }
      | '{-# INLINE CONLIKE' activation qvar '#-}'     { InlineConlikeSig ($1 <^^> $4 <** [$1,$4]) $2 $3 }
      | '{-# SPECIALISE' activation qvar '::' sigtypes '#-}'
            { SpecSig ($1 <^^> $6 <** ($1: $4 : snd $5 ++ [$6])) $2 $3 (fst $5) }
      | '{-# SPECIALISE INLINE' activation qvar '::' sigtypes '#-}'
            { let Loc l (SPECIALISE_INLINE s) = $1
               in SpecInlineSig (l <^^> $6 <** (l:$4:snd $5++[$6])) s $2 $3 (fst $5) }
      | '{-# SPECIALISE' 'instance' ctype '#-}'        {% do { ih <- checkInstHeader $3;
                                                               let {l = $1 <^^> $4 <** [$1,$2,$4]};
                                                               return $ InstSig l ih } }
      | '{-# MINIMAL' name_boolformula '#-}'           { MinimalPragma ($1 <^^> $3 <** [$1,$3]) $2 }

sigtypes :: { ([Type L],[S]) }
      : sigtype                           { ([$1],[]) }
      | sigtype ',' sigtypes              { ($1 : fst $3, $2 : snd $3) }

sigtype :: { Type L }
      : ctype                             {% checkType $ mkTyForall (ann $1) Nothing Nothing $1 }

name_boolformula :: { Maybe (BooleanFormula L) }
       : name_boolformula1         { Just $1 }
       | {- empty -}               { Nothing }

name_boolformula1 :: { BooleanFormula L }
       : name_boolformula_and                       { $1 }
       | name_boolformula_and '|' name_boolformula1 { OrFormula (ann $1 <++>  ann $3 <** [$2]) [$1,$3] }

name_boolformula_and :: { BooleanFormula L }
       : name_boolformula_atom                             { $1 }
       | name_boolformula_atom ',' name_boolformula_and    { AndFormula (ann $1 <++> ann $3 <** [$2]) [$1,$3] }

name_boolformula_atom :: { BooleanFormula L }
       : '(' name_boolformula1 ')' { ParenFormula ($1 <^^> $3 <** [$1,$3]) $2 }
       | var                       { VarFormula (ann $1) $1 }

binds :: { Binds L }
      : decllist                      { $1 }
      | '{' ipbinds '}'               { IPBinds ($1 <^^> $3 <** snd $2) (fst $2) }
      | open ipbinds close            { let l' =  ann . last $ fst $2
                                         in IPBinds (nIS $1 <++> l' <** snd $2) (fst $2) }

vars  :: { ([Name L],[S],L) }
      : vars ',' var                  { let (ns,ss,l) = $1 in ($3 : ns, $2 : ss, l <++> ann $3) }
      | qvar                          {% do { n <- checkUnQual $1;
                                              return ([n],[],ann n) } }

-----------------------------------------------------------------------------
-- | FFI

callconv :: { CallConv L }
         : 'stdcall'                  { StdCall    (nIS $1) }
         | 'ccall'                    { CCall      (nIS $1) }
         | 'cplusplus'                { CPlusPlus  (nIS $1) }
         | 'dotnet'                   { DotNet     (nIS $1) }
         | 'jvm'                      { Jvm        (nIS $1) }
         | 'js'                       { Js         (nIS $1) }
         | 'javascript'               { JavaScript (nIS $1) }
         | 'capi'                     { CApi       (nIS $1) }

fspec :: { (Maybe String, Name L, Type L, [S]) }
      : STRING var_no_safety '::' truedtype               { let Loc l (StringTok (s,_)) = $1 in (Just s, $2, $4, [l,$3]) }
      |        var_no_safety '::' truedtype               { (Nothing, $1, $3, [$2]) }

-----------------------------------------------------------------------------
-- | Pragmas

rules :: { [Rule L] }
      : rules ';'rule         { $3 : $1 }
      | rules ';'             { $1 }
      | rule                  { [$1] }
      | {- empty -}           { [] }

rule :: { Rule L }
     : STRING activation ruleforall exp0 '=' trueexp      {% do { let {Loc l (StringTok (s,_)) = $1};
                                                                  e <- checkRuleExpr $4;
                                                                  return $ Rule (nIS l <++> ann $6 <** l:snd $3 ++ [$5]) s $2 (fst $3) e $6 } }

activation :: { Maybe (Activation L) }
       : {- empty -}          { Nothing }
       | '[' INT ']'          { let Loc l (IntTok (i,_)) = $2 in Just $ ActiveFrom  ($1 <^^> $3 <** [$1,l,$3])    (fromInteger i) }
       | '[' '~' INT ']'      { let Loc l (IntTok (i,_)) = $3 in Just $ ActiveUntil ($1 <^^> $4 <** [$1,$2,l,$4]) (fromInteger i) }

ruleforall :: { (Maybe [RuleVar L],[S]) }
      : {- empty -}                           { (Nothing,[]) }
      | 'forall' rulevars '.'                 { (Just $2,[$1,$3]) }

rulevars :: { [RuleVar L] }
      : rulevar                       { [$1] }
      | rulevar rulevars              { $1 : $2 }

rulevar :: { RuleVar L }
      : varid                             { RuleVar (ann $1) $1 }
      | '(' varid '::' truectype ')'      { TypedRuleVar ($1 <^^> $5 <** [$1,$3,$5]) $2 $4 }

warndeprs :: { ([([Name L],String)],[S]) }
  : warndeprs ';' warndepr            { (fst $3 : fst $1, snd $1 ++ ($2:snd $3)) }
  | warndeprs ';'                     { (fst $1, snd $1 ++ [$2]) }
  | warndepr                          { ([fst $1],snd $1) }
  | {- empty -}                       { ([],[]) }

warndepr :: { (([Name L], String),[S]) }
      : namevars STRING               { let Loc l (StringTok (s,_)) = $2 in ((fst $1,s),snd $1 ++ [l]) }

namevars :: { ([Name L],[S]) }
          : namevar                   { ([$1],[]) }
          | namevar ',' namevars      { ($1 : fst $3, $2 : snd $3) }

namevar :: { Name L }
        : con                         { $1 }
        | var                         { $1 }

annotation :: { Annotation L }
      : 'type' conid aexp             {% checkExpr $3 >>= \e -> return (TypeAnn   (nIS $1 <++> ann e <** [$1]) $2 e) }
      | 'module' aexp                 {% checkExpr $2 >>= \e -> return (ModuleAnn (nIS $1 <++> ann e <** [$1])    e) }
      | namevar aexp                  {% checkExpr $2 >>= \e -> return (Ann ($1 <> e) $1 e) }

-----------------------------------------------------------------------------
-- | Types

truedtype :: { Type L }
      : dtype                         {% checkType $1 }

dtype :: { PType L }
      : dtype_('*',NEVER)              { $1 }

dtype_(ostar,kstar) :: { PType L }
      : btype_(ostar,kstar)                                           { $1 }
      | btype_(ostar,kstar) qtyconop dtype_(ostar,kstar)              { TyInfix ($1 <> $3) $1 $2 $3 }
      | btype_(ostar,kstar) qtyvarop_(ostar) dtype_(ostar,kstar)      { TyInfix ($1 <> $3) $1 (UnpromotedName (ann $2) $2) $3 } -- FIXME
      | btype_(ostar,kstar) '->' ctype_(ostar,kstar)                  { TyFun ($1 <> $3 <** [$2]) ($1) $3 }

truetype :: { Type L }
      : type                          {% checkType $1 }

type :: { PType L }
      : type_('*',NEVER)              { $1 }

type_(ostar,kstar) :: { PType L }
      : ivar '::' dtype_(ostar,kstar) { let l = ($1 <> $3 <** [$2]) in TyPred l $ IParam l $1 $3 }
      | dtype_(ostar,kstar)           { $1 }

truebtype :: { Type L }
      : btype                         {% checkType ($1) }
trueatype :: { Type L }
      : atype                         {% checkType $1 }

btype :: { PType L }
      : btype_('*',NEVER)             { $1 }

btype_(ostar,kstar) :: { PType L }
      : btype_(ostar,kstar) atype_(ostar,kstar) { TyApp ($1 <> $2) $1 $2 }
      | atype_(ostar,kstar)           { $1 }

atype :: { PType L }
      : atype_('*',NEVER)             { $1 }

atype_(ostar,kstar) :: { PType L }
      : kstar                         { TyStar  (nIS $1) }
      | gtycon_(ostar)                { TyCon   (ann $1) $1 }
      | tyvar                         {% checkTyVar $1 }
      | '(' types_(ostar,kstar) ')'                   { TyTuple ($1 <^^> $3 <** ($1:reverse ($3:snd $2))) Boxed   (reverse (fst $2)) }
      | '(#' types_bars2(ostar,kstar) '#)'            { TyUnboxedSum ($1 <^^> $3 <** ($1: reverse ($3: snd $2))) (reverse (fst $2))  }
      | '(#' types1_(ostar,kstar) '#)'                { TyTuple ($1 <^^> $3 <** ($1:reverse ($3:snd $2))) Unboxed (reverse (fst $2)) }
      | '[' type_(ostar,kstar) ']'                    { TyList  ($1 <^^> $3 <** [$1,$3]) $2 }
      | '(' ctype_(ostar,kstar) ')'                   { TyParen ($1 <^^> $3 <** [$1,$3]) $2 }
      | '(' ctype_(ostar,kstar) '::' kind ')'         { TyKind  ($1 <^^> $5 <** [$1,$3,$5]) $2 $4 }
      | '_'                           { TyWildCard (nIS $1) Nothing }
      | ptype_(ostar,kstar)           { % checkEnabled DataKinds >> return (TyPromoted (ann $1) $1) }

ptype_(ostar,kstar) :: { Promoted L }
      :          '[' types_(ostar,kstar)  ']' {% PromotedList  ($1 <^^> $3 <** ($1:reverse($3:snd $2))) False . reverse <\$> mapM checkType (fst $2) }
      | INT                           { let Loc l (IntTok  (i,raw)) = $1 in PromotedInteger (nIS l) i raw }
      | STRING                        { let Loc l (StringTok (s,raw)) = $1 in PromotedString (nIS l) s raw }

gtycon :: { QName L }
      : gtycon_('*')                     { $1 }

gtycon_(ostar) :: { QName L }
      : otycon_(ostar)                     { $1 }
      | '(' ')'                       { unit_tycon_name              ($1 <^^> $2 <** [$1,$2]) }
      | '(' '->' ')'                  { fun_tycon_name               ($1 <^^> $3 <** [$1,$2,$3]) }
      | '[' ']'                       { list_tycon_name              ($1 <^^> $2 <** [$1,$2]) }
      | '(' commas ')'                { tuple_tycon_name             ($1 <^^> $3 <** ($1:reverse $2 ++ [$3])) Boxed (length $2) }
      | '(#' '#)'                     { unboxed_singleton_tycon_name ($1 <^^> $2 <** [$1,$2]) }
      | '(#' commas '#)'              { tuple_tycon_name             ($1 <^^> $3 <** ($1:reverse $2 ++ [$3])) Unboxed (length $2) }

otycon :: { QName L }
      : otycon_('*')                     { $1 }

otycon_(ostar) :: { QName L }
      : qconid                        { $1 }
      | '(' gconsym ')'               { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }
      | '(' qvarsym_(ostar) ')'       { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

qtyconop :: { MaybePromotedName L }
      : qconop                        { UnpromotedName (ann $1) $1 }

truectype :: { Type L }
      : ctype                         {% checkType $1 }

ctype :: { PType L }
      : ctype_('*',NEVER)             { $1 }

ctype_(ostar,kstar) :: { PType L }
      : 'forall' ktyvars '.' ctype_(ostar,kstar)      { mkTyForall (nIS $1 <++> ann $4 <** [$1,$3]) (Just (reverse (fst $2))) Nothing $4 }
      | context_(ostar,kstar) ctype_(ostar,kstar)     { mkTyForall ($1 <> $2) Nothing (Just $1) $2 }
      | type_(ostar,kstar)                            { $1 }

context :: { PContext L }
      : context_('*',NEVER)           { $1 }

context_(ostar,kstar) :: { PContext L }
      : btype_(ostar,kstar) '=>'      {% checkPContext $ (amap (\l -> l <++> nIS $2 <** (srcInfoPoints l ++ [$2]))) ($1) }

types :: { ([PType L],[S]) }
      : types_('*',NEVER)             { $1 }

types_(ostar,kstar) :: { ([PType L],[S]) }
      : types1_(ostar,kstar) ',' ctype_(ostar,kstar)  { ($3 : fst $1, $2 : snd $1)  }

types1 :: { ([PType L],[S]) }
      : types1_('*',NEVER)            { $1 }

types1_(ostar,kstar) :: { ([PType L],[S]) }
      : ctype_(ostar,kstar)                           { ([$1],[]) }
      | types1_(ostar,kstar) ',' ctype_(ostar,kstar)  { ($3 : fst $1, $2 : snd $1) }

types_bars2(ostar,kstar) :: { ([PType L],[S]) }
  : ctype_(ostar,kstar) '|' ctype_(ostar,kstar)       { ([$3, $1], [$2]) }
  | types_bars2(ostar,kstar) '|' ctype_(ostar,kstar)  { ($3 : fst $1, $2 : snd $1) }

ktyvars :: { ([TyVarBind L],Maybe L) }
      : ktyvars ktyvar                { ($2 : fst $1, Just (snd $1 <?+> ann $2)) }
      | {- empty -}                   { ([],Nothing) }

ktyvar :: { TyVarBind L }
      : tyvar                         { UnkindedVar (ann $1) $1 }
      | '(' tyvar '::' kind ')'       { KindedVar ($1 <^^> $5 <** [$1,$3,$5]) $2 $4 }

tyvars :: { ([Name L],Maybe L) }
      : tyvars tyvar                  { ($2 : fst $1, Just (snd $1 <?+> ann $2)) }
      | {- empty -}                   { ([], Nothing) }

tyvars1 :: { ([Name L],L) }
      : tyvars tyvar                  { ($2 : fst $1, snd $1 <?+> ann $2) }

-----------------------------------------------------------------------------
-- | Functional Dependencies

fds :: { ([FunDep L],[S],Maybe L) }
      : {- empty -}                   { ([],[], Nothing) }
      | '|' fds1                      {% do { checkEnabled FunctionalDependencies ;
                                              let {(fds,ss,l) = $2} ;
                                              return (reverse fds, $1 : reverse ss, Just (nIS $1 <++> l)) } }

fds1 :: { ([FunDep L],[S],L) }
      : fds1 ',' fd                   { let (fds,ss,l) = $1 in ($3 : fds, $2 : ss, l <++> ann $3) }
      | fd                            { ([$1],[],ann $1) }

fd :: { FunDep L }
      : tyvars '->' tyvars1            { FunDep (snd $1 <?+> nIS $2 <++> snd $3 <** [$2]) (reverse (fst $1)) (reverse (fst $3)) }

-----------------------------------------------------------------------------
-- | Datatype declarations

-- GADTs - require the GADTs extension enabled, but we handle that at the calling site.

gadtlist :: { ([GadtDecl L],[S],Maybe L) }
      : 'where' '{' gadtconstrs1 '}'                  {% return (fst $3, $1 : $2 : snd $3 ++ [$4], Just $ $1 <^^> $4) }
      | 'where' open gadtconstrs1 close               {% return (fst $3, $1 : $2 : snd $3 ++ [$4], Just $ $1 <^^> $4) }
      | {- empty -}                                   {% checkEnabled EmptyDataDecls >> return ([],[],Nothing) }

gadtconstrs1 :: { ([GadtDecl L],[S]) }
      : optsemis gadtconstrs optsemis         { (fst $2, reverse $1 ++ snd $2 ++ reverse $3)  }

gadtconstrs :: { ([GadtDecl L],[S]) }
      : gadtconstrs semis gadtconstr          { ($3 ++ fst $1, snd $1 ++ reverse $2) }
      | gadtconstr                            { ($1,[]) }

gadtconstr :: { [GadtDecl L] }
      : qcon '::' truectype            {% do { c <- checkUnQual $1;
                                               return [GadtDecl ($1 <> $3 <** [$2]) c Nothing Nothing Nothing $3] } }
      | qcon '::' context '{' fielddecls '}' '->' truectype
                                      {% do { c <- checkUnQual $1;
                                              ctxt <- checkContext (Just $3) ;
                                              return [GadtDecl ($1 <> $8 <** [$2,$4,$6,$7] ++ snd $5) c Nothing ctxt (Just (reverse $ fst $5)) $8] } }
      | qcon '::' '{' fielddecls '}' '->' truectype
                                      {% do { c <- checkUnQual $1;
                                              return [GadtDecl ($1 <> $7 <** [$2,$3,$5,$6] ++ snd $4) c Nothing Nothing (Just (reverse $ fst $4)) $7] } }

constrs0 :: { ([QualConDecl L],[S],Maybe L) }
      : '=' constrs                   { let (ds,ss,l) = $2 in (ds, $1 : reverse ss, Just $ nIS $1 <++> l) }

constrs :: { ([QualConDecl L],[S],L) }
      : constrs '|' constr            { let (ds,ss,l) = $1 in ($3 : ds, $2 : ss, l <++> ann $3) }
      | constr                        { ([$1],[],ann $1) }

constr :: { QualConDecl L }
      : forall context constr1        {% do { checkEnabled ExistentialQuantification ;
                                               ctxt <- checkContext (Just $2) ;
                                               let {(mtvs,ss,ml) = $1} ;
                                               return $ QualConDecl (ml <?+> ann $3 <** ss) mtvs ctxt $3 } }
      | forall constr1                 { let (mtvs, ss, ml) = $1 in QualConDecl (ml <?+> ann $2 <** ss) mtvs Nothing $2 }

forall :: { (Maybe [TyVarBind L], [S], Maybe L) }
      : 'forall' ktyvars '.'          {% checkEnabled ExistentialQuantification >> return (Just (fst $2), [$1,$3], Just $ $1 <^^> $3) }
      | {- empty -}                   { (Nothing, [], Nothing) }

constr1 :: { ConDecl L }
      : scontype                      { let (n,ts,l) = $1 in ConDecl l n ts }
      | truebtype conop truebtype     { InfixConDecl ($1 <> $3) $1 $2 $3 }
      | qcon '{' '}'                  {% do { c <- checkUnQual $1; return $ RecDecl (ann $1 <++> nIS $3 <** [$2,$3]) c [] } }
      | qcon '{' fielddecls '}'       {% do { c <- checkUnQual $1;
                                              return $ RecDecl (ann $1 <++> nIS $4 <** ($2:reverse (snd $3) ++ [$4])) c (reverse (fst $3)) } }

scontype :: { (Name L, [Type L], L) }
      : btype                         {% do { (c,ts) <- splitTyConApp $1;
                                              return (c, ts, ann $1) } }

fielddecls :: { ([FieldDecl L],[S]) }
      : fielddecls ',' fielddecl      { ($3 : fst $1, $2 : snd $1) }
      | fielddecl                     { ([$1],[]) }

fielddecl :: { FieldDecl L }
      : vars '::' truectype               { let (ns,ss,l) = $1 in FieldDecl (l <++> ann $3 <** (reverse ss ++ [$2])) (reverse ns) $3 }

maybe_derivings :: { [Deriving L] }
      : {- empty -}                   { [] }
      | derivings                     { $1 }

derivings :: { [Deriving L] }
      : derivings deriving            { $2 : $1 }
      | deriving                      { [$1] }

deriving :: { Deriving L }
      : 'deriving' deriv_clause_types
            { let (ihs, last_ss, sss) = $2
              in Deriving ($1 <^^> last_ss <** $1:sss) Nothing ihs }
      | 'deriving' deriv_strategy_no_via deriv_clause_types
            { let (ihs, last_ss, sss) = $3
              in Deriving ($1 <^^> last_ss <** $1:sss) (Just $2) ihs }
      | 'deriving' deriv_clause_types deriv_strategy_via
            { let (ihs, last_ss, sss) = $2
              in Deriving ($1 <^^> last_ss <** $1:sss) (Just $3) ihs }

dclasses :: { ([InstRule L],[S]) }
      : types1                        {% checkDeriving (fst $1) >>= \ds -> return (ds, snd $1) }

qtycls1 :: { InstHead L }
      : qconid                        { IHCon (ann $1) $1 }

deriv_clause_types :: { ([InstRule L], SrcSpan, [SrcSpan]) }
      : qtycls1                       { [IRule (ann $1) Nothing Nothing $1], srcInfoSpan (ann $1), [] }
      | '(' ')'                       { [], $2, [$1, $2] }
      | '(' dclasses ')'              { case fst $2 of
                                          [ts] -> ([IParen ($1 <^^> $3 <** [$1,$3]) ts], $3, [])
                                          tss  -> (reverse tss, $3, $1: reverse (snd $2) ++ [$3]) }

-----------------------------------------------------------------------------
-- | Kinds

kind :: { Kind L }
      : kind1                 {% checkEnabled KindSignatures >> return $1 }

kind1 :: { Kind L }
      : ctype_(NEVER,'*')     {% checkType $1 }

optkind :: { (Maybe (Kind L), [S]) }
      : {-empty-}             { (Nothing,[]) }
      | '::' kind             { (Just $2,[$1]) }

opt_tyconsig :: { Maybe ( S, QName L ) }
             : {- empty -}    { Nothing }
             | '::' gtycon    { Just ($1, $2) }

-----------------------------------------------------------------------------
-- | Class declarations

optcbody :: { (Maybe [ClassDecl L],[S],Maybe L) }
      : 'where' '{'  cldecls '}'      {% checkClassBody (fst $3) >>= \vs -> return (Just vs, $1:$2: snd $3 ++ [$4], Just ($1 <^^> $4)) }
      | 'where' open cldecls close    {% do { vs <- checkClassBody (fst $3);
                                              let { l' = if null (fst $3) then nIS $4 else (ann . last $ fst $3) };
                                              return (Just vs, $1:$2: snd $3 ++ [$4], Just (nIS $1 <++> l')) } }
      | {- empty -}                   { (Nothing,[],Nothing) }

cldecls :: { ([ClassDecl L],[S]) }
      : optsemis cldecls1 optsemis    {% checkRevClsDecls (fst $2) >>= \cs -> return (cs, reverse $1 ++ snd $2 ++ reverse $3) }
      | optsemis                      { ([],reverse $1) }

cldecls1 :: { ([ClassDecl L],[S]) }
      : cldecls1 semis cldecl         { ($3 : fst $1, snd $1 ++ reverse $2) }
      | cldecl                        { ([$1],[]) }

cldecl :: { ClassDecl L }
      : decl                          { ClsDecl (ann $1) $1 }
      | atdecl                        {% checkEnabled TypeFamilies >> return $1 }
      | 'default' signdecl            {% checkEnabled DefaultSignatures >> checkDefSigDef $2 >>= \(n,t,l) -> return (ClsDefSig (nIS $1 <++> ann $2 <** [$1,l]) n t) }

opt_family   :: { [S] }
             : {- empty -}   { [] }
             | 'family'      { [$1] }

atdecl :: { ClassDecl L }
      : 'data' opt_family type opt_datafam_kind_sig
            {% do { (cs,dh) <- checkDataHeader $3;
                    return (ClsDataFam (nIS $1 <++> ann $3 <+?> (fmap ann) $4 <** [$1]) cs dh $4) } }
      | 'type' type opt_at_kind_inj_sig2
            {% mkAssocType $1 $2 $3 }
      | 'type' 'family' type opt_at_kind_inj_sig
            {% do { dh <- checkSimpleType $3;
                    return (ClsTyFam  (nIS $1 <++> ann $3 <+?> (fmap ann) (fst $4)
                                                          <+?> (fmap ann) (snd $4)
                                                          <** [$1]) dh (fst $4) (snd $4)) } }
      | 'type' 'instance' ty_fam_inst_eqn
                    { ClsTyDef (nIS $1 <++> ann $3 <** [$1,$2]) $3 }

-----------------------------------------------------------------------------
-- | Instance declarations

optvaldefs :: { (Maybe [InstDecl L],[S],Maybe L) }
      : 'where' '{'  valdefs '}'      {% checkInstBody (fst $3) >>= \vs -> return (Just vs, $1:$2: snd $3 ++ [$4], Just ($1 <^^> $4))  }
      | 'where' open valdefs close    {% checkInstBody (fst $3) >>= \vs -> return (Just vs, $1:$2: snd $3 ++ [$4], Just ($1 <^^> $4)) }
      | {- empty -}                   { (Nothing, [], Nothing) }

valdefs :: { ([InstDecl L],[S]) }
      : optsemis valdefs1 optsemis    {% checkRevInstDecls (fst $2) >>= \is -> return (is, reverse $1 ++ snd $2 ++ reverse $3) }
      | optsemis                      { ([],reverse $1) }

valdefs1 :: { ([InstDecl L],[S]) }
      : valdefs1 semis insvaldef      { ($3 : fst $1, snd $1 ++ reverse $2) }
      | insvaldef                     { ([$1],[]) }

insvaldef :: { InstDecl L }
      : valdef                        { InsDecl (ann $1) $1 }
      | atinst                        {% checkEnabled TypeFamilies >> return $1 }
      | specinldecl                   { InsDecl (ann $1) $1 }
      | signdecl0                     {% checkEnabled InstanceSigs >> return (InsDecl (ann $1) $1) }

atinst :: { InstDecl L }
      : 'type' truedtype '=' truectype
               {% do { -- no checkSimpleType $4 since dtype may contain type patterns
                       return (InsType (nIS $1 <++> ann $4 <** [$1,$3]) $2 $4) } }
      | data_or_newtype truectype constrs0 maybe_derivings
               {% do { -- (cs,c,t) <- checkDataHeader $4;
                       let {(ds,ss,minf) = $3};
                       checkDataOrNew $1 ds;
                       return (InsData ($1 <> $2 <+?> minf <+?> fmap ann (listToMaybe $4) <** ss ) $1 $2 (reverse ds) (reverse $4)) } }
      | data_or_newtype truectype optkind gadtlist maybe_derivings
               {% do { -- (cs,c,t) <- checkDataHeader $4;
                       let { (gs,ss,minf) = $4 } ;
                       checkDataOrNewG $1 gs;
                       return $ InsGData (ann $1 <+?> minf <+?> fmap ann (listToMaybe $5) <** (snd $3 ++ ss)) $1 $2 (fst $3) (reverse gs) (reverse $5) } }

-----------------------------------------------------------------------------
-- | Value definitions

valdef :: { Decl L }
      : exp0b optsig rhs optwhere     {% checkValDef (($1 <> $3 <+?> (fmap ann) (fst $4)) <** (snd $4)) $1 $2 $3 (fst $4) }

optwhere :: { (Maybe (Binds L),[S]) }
      : 'where' binds                 { (Just $2, [$1]) }
      | {- empty -}                   { (Nothing, []) }

optsig :: { (Maybe (Type L, S)) }
      : '::' truectype                {% checkEnabled ScopedTypeVariables >> return (Just ($2, $1)) }
      | {- empty -}                   { Nothing }

rhs   :: { Rhs L }
      : '=' trueexp                   { UnGuardedRhs (nIS $1 <++> ann $2 <** [$1]) $2 }
      | gdrhs                         { GuardedRhss (snd $1) (reverse $ fst $1) }

gdrhs :: { ([GuardedRhs L],L) }
      : gdrhs gdrh                    { ($2 : fst $1, snd $1 <++> ann $2) }
      | gdrh                          { ([$1],ann $1) }

gdrh :: { GuardedRhs L }
      : '|' quals '=' trueexp  {% do { checkPatternGuards (fst $2);
                                       return $ GuardedRhs (nIS $1 <++> ann $4 <** ($1:snd $2 ++ [$3])) (reverse (fst $2)) $4 } }

-----------------------------------------------------------------------------
-- | Expressions

trueexp :: { Exp L }
        : exp                 {% checkExpr $1 }

exp   :: { PExp L }
      : exp0b '::' truectype              { ExpTypeSig      ($1 <> $3 <** [$2]) $1 $3 }
      | exp0                              { $1 }
      | exp0b qop                         { PostOp          ($1 <> $2)          $1 $2 }

exp0 :: { PExp L }
      : exp0a                         { $1 }
      | exp0b                         { $1 }

exp0a :: { PExp L }
      : exp0b qop exp10a              { InfixApp ($1 <> $3) $1 $2 $3 }
      | exp10a                        { $1 }

exp0b :: { PExp L }
      : exp0b qop exp10b              { InfixApp ($1 <> $3) $1 $2 $3 }
      | exp10b                        { $1 }

exp10a :: { PExp L }
      : expblocka                     { $1 }
      | fexp expblocka                {% checkEnabled BlockArguments >>
                                         return (App ($1 <> $2) $1 $2) }

expblocka :: { PExp L }
      : 'fun' apats '->' exp             { Lambda (nIS $1 <++> ann $4 <** [$1,$3]) (reverse $2) $4 }
      | 'let' binds 'in' exp            { Let    (nIS $1 <++> ann $4 <** [$1,$3])    $2 $4 }
      | 'if' exp optlayoutsemi 'then' exp optlayoutsemi 'else' exp
                                       { If     (nIS $1 <++> ann $8 <** ($1:$3 ++ $4:$6 ++ [$7])) $2 $5 $8 }
      | 'if' ifaltslist                 {% checkEnabled MultiWayIf >>
                                           let (alts, inf, ss) = $2
                                           in return (MultiIf (nIS $1 <++> inf <** ($1:ss)) alts) }
      | exppragma                       { $1 }

optlayoutsemi :: { [S] }
      : ';'				  {% checkEnabled DoAndIfThenElse >> return [$1] }
      | {- empty -}			  { [] }

opthsxsemi :: { [S] }
      : ';'		    { [$1] }
      | {- empty -} { [] }

exp10b :: { PExp L }
      : expblockb                     { $1 }
      | '-' fexp                      { NegApp (nIS $1 <++> ann $2 <** [$1]) $2 }
      | fexp                          { $1 }

expblockb :: { PExp L }
      : 'case' exp 'of' altslist      { let (als, inf, ss) = $4 in Case (nIS $1 <++> inf <** ($1:$3:ss)) $2 als }
      | 'do'  stmtlist                { let (sts, inf, ss) = $2 in Do   (nIS $1 <++> inf <** $1:ss) sts }
      | 'mdo' stmtlist                { let (sts, inf, ss) = $2 in MDo  (nIS $1 <++> inf <** $1:ss) sts }

exppragma :: { PExp L }
      : '{-# CORE' STRING '#-}' exp   { let Loc l (StringTok (s,_)) = $2 in CorePragma (nIS $1 <++> ann $4 <** [l,$3]) s $4 }
      | '{-# SCC'  STRING '#-}' exp   { let Loc l (StringTok (s,_)) = $2 in SCCPragma  (nIS $1 <++> ann $4 <** [l,$3]) s $4 }
      | '{-# GENERATED' STRING INT ':' INT '-' INT ':' INT '#-}' exp
                                          { let { Loc l0 (StringTok (s,_)) = $2;
                                                  Loc l1 (IntTok (i1,_))   = $3;
                                                  Loc l2 (IntTok (i2,_))   = $5;
                                                  Loc l3 (IntTok (i3,_))   = $7;
                                                  Loc l4 (IntTok (i4,_))   = $9}
                                             in GenPragma (nIS $1 <++> ann $11 <** [$1,l0,l1,$4,l2,$6,l3,$8,l4,$10])
                                                      s (fromInteger i1, fromInteger i2)
                                                        (fromInteger i3, fromInteger i4) $11 }

fexp :: { PExp L }
      : fexp aexp                     { App ($1 <> $2) $1 $2 }
      | fexp expblockb                {% checkEnabled BlockArguments >>
                                         return (App ($1 <> $2) $1 $2) }
      | aexp                          { $1 }

apats :: { [Pat L] }
      : apats apat                    { $2 : $1 }
      | apat                          { [$1] }

apat :: { Pat L }
      : aexp                          {% checkPattern $1 }

aexp  :: { PExp L }
      : qvar '@' aexp                 {% do { n <- checkUnQual $1;
                                              return (AsPat ($1 <> $3 <** [$2]) n $3) } }
      | '~' aexp                      { IrrPat (nIS $1 <++> ann $2 <** [$1]) $2 }
      | aexp1                         { $1 }

-- Note: The first two alternatives of aexp1 are not necessarily record
-- updates: they could be labeled constructions.

aexp1 :: { PExp L }
      : aexp1 '{' '}'                 {% liftM (amap (const (ann $1 <++> nIS $3 <** [$2,$3]))) $ mkRecConstrOrUpdate $1 [] }
      | aexp1 '{' fbinds '}'          {% liftM (amap (const (ann $1 <++> nIS $4 <** ($2:snd $3 ++ [$4]))))
                                              $ mkRecConstrOrUpdate $1 (fst $3) }
      | aexp2                         { $1 }

-- According to the Report, the left section (e op) is legal iff (e op x)
-- parses equivalently to ((e) op x).  Thus e must be an exp0b.
-- An implicit parameter can be used as an expression, enabled by the lexer.
-- Extensions using banana brackets are also enabled by the lexer. The only
-- thing we need to look at here is the erpats that use no non-standard lexemes.

aexp2 :: { PExp L }
      : ivar                          { IPVar (ann $1) $1 }
      | overloaded_label              { $1 }
      | qvar                          { Var (ann $1) $1 }
      | gcon                          { $1 }
      | literal                       { Lit (ann $1) $1 }
      | '(' texp ')'                  { Paren ($1 <^^> $3 <** [$1,$3]) $2 }
      | '(' tup_exprs ')'             {% do { e <- mkSumOrTuple Boxed ($1 <^^> $3) (snd $2)
                                            ; return $ amap (\l -> l <** [$1] ++ fst $2 ++ [$3]) e } }
      | '(#' texp '#)'                { TupleSection ($1 <^^> $3 <** [$1,$3]) Unboxed [Just $2] }
      | '(#' tup_exprs '#)'         {% do { e <- mkSumOrTuple Unboxed ($1 <^^> $3) (snd $2)
                                          ; return $ amap (\l -> l <** [$1] ++ fst $2 ++ [$3]) e } }
      | '[' list ']'                  { amap (\l -> l <** [$3]) $ $2 ($1 <^^> $3 <** [$1]) }

tup_exprs :: { ([S], SumOrTuple L) }
      : texp commas_tup_tail          { (fst $2, STuple (Just $1 : snd $2)) }
      | texp bars                     { ($2, SSum 0 (length $2) $1) }
      | commas tup_tail               { ($1 ++ (fst $2), STuple ((map (const Nothing) $1) ++ snd $2)) }
      | bars texp bars0               { ($1 ++ $3, SSum (length $1) (length $3) $2) }

commas_tup_tail :: { ([S], [Maybe (PExp L)]) }
      : commas tup_tail               { (reverse $1 ++ fst $2, map (const Nothing) (tail $1) ++ snd $2) }

tup_tail :: { ([S], [Maybe (PExp L)]) }
          : texp commas_tup_tail { (fst $2, Just $1 : snd $2) }
          | texp                 { ([], [Just $1]) }
          | {- empty -}          { ([], [Nothing]) }

commas :: { [S] }
      : commas ','                    { $2 : $1 }
      | ','                           { [$1] }

bars :: { [S] }
  : bars '|'  { $2 : $1 }
  | '|'       { [$1] }

bars0 :: { [S] }
       : bars { $1 }
       |      { [] }

texp :: { PExp L }
      : exp                           { $1 }
      | qopm exp0                     { PreOp ($1 <> $2) $1 $2 }
      | exp '->' pat                  {% do {checkEnabled ViewPatterns;
                                             return $ ViewPat ($1 <> $3 <** [$2]) $1 $3} }

-----------------------------------------------------------------------------
-- | List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.

list :: { L -> PExp L }
      : texp                          { \l -> List l [$1] }
      | lexps                         { \l -> let (ps,ss) = $1 in List (l <** reverse ss) (reverse ps) }
      | texp '..'                     { \l -> EnumFrom       (l <** [$2]) $1 }
      | texp ',' exp '..'             { \l -> EnumFromThen   (l <** [$2,$4]) $1 $3 }
      | texp '..' exp                 { \l -> EnumFromTo     (l <** [$2]) $1 $3 }
      | texp ',' exp '..' exp         { \l -> EnumFromThenTo (l <** [$2,$4]) $1 $3 $5 }
      | texp '|' pqualstmts           { \l -> let (stss, ss) = $3 in ParComp (l <** ($2:ss)) $1 (reverse stss) }

lexps :: { ([PExp L],[S]) }
      : lexps ',' texp                { let (es, ss) = $1 in ($3 : es, $2 : ss) }
      | texp ',' texp                 { ([$3,$1], [$2]) }

-----------------------------------------------------------------------------
-- | List comprehensions

pqualstmts :: { ([[QualStmt L]],[S]) }
      : pqualstmts '|' qualstmts      { let { (stss, ss1) = $1;
                                              (sts, ss2) = $3 }
                                         in (reverse sts : stss, ss1 ++ [$2] ++ reverse ss2)  }
      | qualstmts                     { let (sts, ss) = $1 in ([reverse sts], reverse ss) }

qualstmts :: { ([QualStmt L],[S]) }
      : qualstmts ',' qualstmt        { let (sts, ss) = $1 in ($3 : sts, $2 : ss) }
      | qualstmt                      { ([$1],[]) }

qualstmt :: { QualStmt L }
      : qual                          { QualStmt (ann $1) $1 }

quals :: { ([Stmt L],[S]) }
      : quals ',' qual                { let (sts, ss) = $1 in ($3 : sts, $2 : ss) }
      | qual                          { ([$1],[]) }

qual  :: { Stmt L }
      : pat '<-' trueexp              { Generator ($1 <> $3 <** [$2]) $1 $3 }
      | trueexp                       { Qualifier (ann $1) $1 }
      | 'let' binds                   { LetStmt   (nIS $1 <++> ann $2 <** [$1]) $2 }

-----------------------------------------------------------------------------
-- | Case alternatives

altslist :: { ([Alt L],L,[S]) }
      : '{'  alts '}'                 { (fst $2, $1 <^^> $3, $1:snd $2 ++ [$3])  }
      | open alts close               { let l' =  ann . last $ fst $2
                                         in (fst $2, nIS $1 <++> l', $1:snd $2 ++ [$3]) }
      | '{' '}'                       {% do { checkEnabled EmptyCase;
                                              return ([], $1 <^^> $2, [$1, $2]) } }

alts :: { ([Alt L],[S]) }
      : optsemis alts1 optsemis       { (reverse $ fst $2, $1 ++ snd $2 ++ $3) }

alts1 :: { ([Alt L],[S]) }
      : alts1 semis alt               { ($3 : fst $1, snd $1 ++ $2) }
      | alt                           { ([$1],[]) }

alt :: { Alt L }
      : pat ralt optwhere             { Alt ($1 <> $2 <+?> (fmap ann) (fst $3) <** snd $3) $1 $2 (fst $3) }

ralt :: { Rhs L }
      : '->' trueexp                  { UnGuardedRhs (nIS $1 <++> ann $2 <** [$1]) $2 }
      | gdpats                        { GuardedRhss  (snd $1) (reverse $ fst $1) }

gdpats :: { ([GuardedRhs L],L) }
      : gdpats gdpat                  { ($2 : fst $1, snd $1 <++> ann $2) }
      | gdpat                         { ([$1], ann $1) }

gdpat :: { GuardedRhs L }
      : '|' quals '->' trueexp {% do { checkPatternGuards (fst $2);
                                       let {l = nIS $1 <++> ann $4 <** ($1:snd $2 ++ [$3])};
                                       return (GuardedRhs l (reverse (fst $2)) $4) } }

pat :: { Pat L }
      : exp                           {% checkPattern $1 }

ifaltslist :: { ([GuardedRhs L], L, [S]) }
      : '{'  ifalts '}'                 { (fst $2, $1 <^^> $3, $1:snd $2 ++ [$3])  }
      | open ifalts close               { let l' =  ann . last $ fst $2
                                           in (fst $2, nIS $1 <++> l', $1:snd $2 ++ [$3]) }

ifalts :: { ([GuardedRhs L], [S]) }
      : optsemis ifalts1 optsemis       { (reverse $ fst $2, $1 ++ snd $2 ++ $3) }

ifalts1 :: { ([GuardedRhs L], [S]) }
      : ifalts1 optsemis gdpat           { ($3 : fst $1, snd $1 ++ $2) }
      | gdpat                            { ([$1], []) }

-----------------------------------------------------------------------------
-- | Statement sequences

stmtlist :: { ([Stmt L],L,[S]) }
      : '{'  stmts '}'                { (fst $2, $1 <^^> $3, $1:snd $2 ++ [$3])  }
      | open stmts close              { let l' =  ann . last $ fst $2
                                         in (fst $2, nIS $1 <++> l', $1:snd $2 ++ [$3]) }

stmts :: { ([Stmt L],[S]) }
      : stmt stmts1                       { ($1 : fst $2, snd $2) }
      | ';' stmts                         { (fst $2, $1 : snd $2) }
      | {- empty -}                       { ([],[]) }

stmts1 :: { ([Stmt L],[S]) }
      : ';' stmts                         { (fst $2, $1 : snd $2) }
      | {- empty -}                       { ([],[]) }

-- A let statement may bind implicit parameters.
stmt :: { Stmt L }
      : 'let' binds                       { LetStmt (nIS $1 <++> ann $2 <** [$1]) $2 }
      | pat '<-' trueexp                  { Generator ($1 <> $3 <** [$2]) $1 $3 }
      | trueexp                           { Qualifier (ann $1) $1 }

-----------------------------------------------------------------------------
-- | Record Field Update/Construction

fbinds :: { ([PFieldUpdate L],[S]) }
      : fbind ',' fbinds              { let (fbs, ss) = $3 in ($1 : fbs, $2 : ss) }
      | fbind                         { ([$1],[]) }
      | '..'                          {% do { checkEnabled RecordWildCards `atSrcLoc` (getPointLoc $1);
                                              return ([FieldWildcard (nIS $1)], []) } }

-- Puns and wild cards need the respective extensions enabled.

fbind :: { PFieldUpdate L }
      : qvar '=' exp                  { FieldUpdate ($1 <>$3 <** [$2]) $1 $3 }
      | qvar                          {% checkEnabled NamedFieldPuns >> checkQualOrUnQual $1 >>= return . FieldPun (ann $1) }

-----------------------------------------------------------------------------
-- | Implicit parameter bindings - need the ImplicitParameter extension enabled, but the lexer handles that.

ipbinds :: { ([IPBind L],[S]) }
      : optsemis ipbinds1 optsemis    { (reverse (fst $2), reverse $1 ++ snd $2 ++ reverse $3) }

ipbinds1 :: { ([IPBind L],[S]) }
      : ipbinds1 semis ipbind         { ($3 : fst $1, snd $1 ++ reverse $2) }
      | ipbind                        { ([$1],[]) }

ipbind :: { IPBind L }
      : ivar '=' trueexp              { IPBind ($1 <> $3 <** [$2]) $1 $3 }

-----------------------------------------------------------------------------
-- | Variables, Constructors and Operators.

gcon :: { PExp L }
      : sysdcon               { $1 }
      | qcon                  { Con (ann $1) $1 }

gcon_nolist :: { PExp L }
      : sysdcon_nolist        { $1 }
      | qcon                  { Con (ann $1) $1 }

sysdcon_nolist :: { PExp L }
      : '(' ')'               { p_unit_con              ($1 <^^> $2 <** [$1,$2]) }
      | '(#' '#)'             { p_unboxed_singleton_con ($1 <^^> $2 <** [$1,$2]) }
      | '(#' commas '#)'      { p_tuple_con             ($1 <^^> $3 <** $1:reverse ($3:$2)) Unboxed (length $2) }
      | '(' commas ')'        { p_tuple_con             ($1 <^^> $3 <** $1:reverse ($3:$2)) Boxed (length $2) }

sysdcon :: { PExp L }
      : '[' ']'               { List                    ($1 <^^> $2 <** [$1,$2]) [] }
      | sysdcon_nolist        { $1 }

var   :: { Name L }
      : varid                 { $1 }
      | '(' varsym ')'        { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

var_no_safety :: { Name L }
      : varid_no_safety       { $1 }
      | '(' varsym ')'        { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

qvar  :: { QName L }
      : qvarid                { $1 }
      | '(' qvarsym ')'       { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

ivar  :: { IPName L }
      : ivarid                { $1 }

con   :: { Name L }
      : conid                 { $1 }
      | '(' consym ')'        { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

con_list :: { ([S], [Name L]) }
         : con                { ([], [$1]) }
         | con ',' con_list   { let (ss, cs) = $3
                                in ($2 : ss, $1 :cs) }

qcon  :: { QName L }
      : qconid                { $1 }
      | '(' gconsym ')'       { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

varop :: { Name L }
      : varsym                { $1 }
      | '`' varid '`'         { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

qvarop :: { QName L }
      : qvarsym               { $1 }
      | '`' qvarid '`'        { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

qvaropm :: { QName L }
      : qvarsymm              { $1 }
      | '`' qvarid '`'        { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

conop :: { Name L }
      : consym                { $1 }
      | '`' conid '`'         { fmap (const ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3])) $2 }

qconop :: { QName L }
      : gconsym               { $1 }
      | '`' qconid '`'        { updateQNameLoc ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }

op    :: { Op L }
      : varop                 { VarOp (ann $1) $1 }
      | conop                 { ConOp (ann $1) $1 }

qop   :: { QOp L }
      : qvarop                { QVarOp (ann $1) $1 }
      | qconop                { QConOp (ann $1) $1 }

qopm  :: { QOp L }
      : qvaropm               { QVarOp (ann $1) $1 }
      | qconop                { QConOp (ann $1) $1 }

gconsym :: { QName L }
      : ':'                   { list_cons_name (nIS $1) }
      | qconsym               { $1 }

overloaded_label :: { PExp L }
      : LABELVARID            { let Loc l (LabelVarId v) = $1 in OverloadedLabel
                                                                     (nIS l) v }

-----------------------------------------------------------------------------
-- | Identifiers and Symbols

qvarid :: { QName L }
      : varid                 { UnQual (ann $1) $1 }
      | QVARID                { let {Loc l (QVarId q) = $1; nis = nIS l}
                                 in Qual nis (ModuleName nis (fst q)) (Ident nis (snd q)) }
      | '_'                   { hole_name       (nIS $1) }

varid_no_safety :: { Name L }
      : VARID                 { let Loc l (VarId v) = $1 in Ident (nIS l) v }
      | 'as'                  { as_name         (nIS $1) }
      | 'qualified'           { qualified_name  (nIS $1) }
      | 'hiding'              { hiding_name     (nIS $1) }
      | 'export'              { export_name     (nIS $1) }
      | 'stdcall'             { stdcall_name    (nIS $1) }
      | 'ccall'               { ccall_name      (nIS $1) }
      | 'cplusplus'           { cplusplus_name  (nIS $1) }
      | 'dotnet'              { dotnet_name     (nIS $1) }
      | 'jvm'                 { jvm_name        (nIS $1) }
      | 'js'                  { js_name         (nIS $1) }
      | 'javascript'          { javascript_name (nIS $1) }
      | 'capi'                { capi_name       (nIS $1) }
      | 'stock'               { stock_name      (nIS $1) }
      | 'anyclass'            { anyclass_name   (nIS $1) }

varid :: { Name L }
      : varid_no_safety       { $1 }
      | 'forall'              { forall_name	  (nIS $1) }
      | 'family'              { family_name     (nIS $1) }

-- Implicit parameter

ivarid :: { IPName L }
      : IDUPID                { let Loc l (IDupVarId i) = $1 in IPDup (nIS l) i }
      | ILINID                { let Loc l (ILinVarId i) = $1 in IPLin (nIS l) i }

qconid :: { QName L }
      : conid                 { UnQual (ann $1) $1 }
      | QCONID                { let {Loc l (QConId q) = $1; nis = nIS l} in Qual nis (ModuleName nis (fst q)) (Ident nis (snd q)) }

conid :: { Name L }
      : CONID                 { let Loc l (ConId c) = $1 in Ident (nIS l) c }

qconsym :: { QName L }
      : consym                { UnQual (ann $1) $1 }
      | QCONSYM               { let {Loc l (QConSym q) = $1; nis = nIS l} in Qual nis (ModuleName nis (fst q)) (Symbol nis (snd q)) }

consym :: { Name L }
      : CONSYM                { let Loc l (ConSym c) = $1 in Symbol (nIS l) c }

qvarsym :: { QName L }
      : qvarsym_('*')         { $1 }

qvarsym_(ostar) :: { QName L }
      : varsym_(ostar)        { UnQual (ann $1) $1 }
      | qvarsym1              { $1 }

qvarsymm :: { QName L }
      : varsymm               { UnQual (ann $1) $1 }
      | qvarsym1              { $1 }

varsym :: { Name L }
      : varsym_('*')          { $1 }

varsym_(ostar) :: { Name L }
      : varsymm_(ostar)       { $1 }
      | '-'                   { minus_name (nIS $1) }

varsymm :: { Name L }
      : varsymm_('*')         { $1 }

varsymm_(ostar) :: { Name L } -- varsym not including '-'
      : VARSYM                { let Loc l (VarSym v) = $1 in Symbol (nIS l) v }
      | '!'                   { bang_name (nIS $1) }
      | '.'                   { dot_name  (nIS $1) }
      | ostar                 { star_name (nIS $1) }

qvarsym1 :: { QName L }
      : QVARSYM               { let {Loc l (QVarSym q) = $1; nis = nIS l} in Qual nis (ModuleName nis (fst q)) (Symbol nis (snd q)) }

literal :: { Literal L }
      : INT                   { let Loc l (IntTok        (i,raw)) = $1 in Int        (nIS l) i raw }
      | CHAR                  { let Loc l (Character     (c,raw)) = $1 in Char       (nIS l) c raw }
      | RATIONAL              { let Loc l (FloatTok      (r,raw)) = $1 in Frac       (nIS l) r raw }
      | STRING                { let Loc l (StringTok     (s,raw)) = $1 in String     (nIS l) s raw }
      | PRIMINT               { let Loc l (IntTokHash    (i,raw)) = $1 in PrimInt    (nIS l) i raw }
      | PRIMWORD              { let Loc l (WordTokHash   (w,raw)) = $1 in PrimWord   (nIS l) w raw }
      | PRIMFLOAT             { let Loc l (FloatTokHash  (f,raw)) = $1 in PrimFloat  (nIS l) f raw }
      | PRIMDOUBLE            { let Loc l (DoubleTokHash (d,raw)) = $1 in PrimDouble (nIS l) d raw }
      | PRIMCHAR              { let Loc l (CharacterHash (c,raw)) = $1 in PrimChar   (nIS l) c raw }
      | PRIMSTRING            { let Loc l (StringHash    (s,raw)) = $1 in PrimString (nIS l) s raw }

-----------------------------------------------------------------------------
-- | Layout

open  :: { S }  :       {% pushCurrentContext >> getSrcLoc >>= \s -> return $ mkSrcSpan s s {- >>= \x -> trace (show x) (return x) -} }

close :: { S }
      : vccurly               { $1 {- >>= \x -> trace (show x ++ show x ++ show x) (return x) -} } -- context popped in lexer.
      | error                 {% popContext >> getSrcLoc >>= \s -> return $ mkSrcSpan s s {- >>= \x -> trace (show x ++ show x) (return x) -} }

-----------------------------------------------------------------------------
-- | Pattern synonyms

pat_syn :: { Decl L }
      : pattern_synonym_decl          {% checkEnabled PatternSynonyms >> return $1 }

-- Glasgow extension: pattern synonyms
 pattern_synonym_decl :: { Decl L }
      : 'pattern' pattern_synonym_lhs '=' pat
           { let l = nIS $1 <++> ann $4 <** [$1,$3]
             in PatSyn l $2 $4 ImplicitBidirectional
                 }
      | 'pattern' pattern_synonym_lhs '<-' pat
           {   let l = nIS $1 <++> ann $4 <** [$1,$3]
               in PatSyn l $2 $4 Unidirectional
           }
      | 'pattern' pattern_synonym_lhs '<-' pat where_decls
          {  let l = nIS $1 <++> ann $4 <** [$1, $3]
             in PatSyn l $2 $4 $5}


pattern_synonym_lhs :: { Pat L }
      : con vars0  { let l = case $2 of
                                 [] -> ann $1
                                 (_:_) -> ann $1 <++> (ann $ last $2)
                        in PApp l (UnQual (ann $1) $1) $2 }
      | varid qconsym varid  { PInfixApp (ann $1 <++> ann $3) (PVar (ann $1) $1) $2 (PVar (ann $3) $3) }
      | con '{' commavars '}' {  let { (ss, ns) = $3 ;
                                       qnames = (map (\n -> UnQual (ann n) n) ns) }
                                 in PRec (ann $1 <++> nIS $4 <** ($2 : ss ++ [$4]))
                                         (UnQual (ann $1) $1) (map (\q -> PFieldPun (ann q) q) qnames) }

vars0 :: { [Pat L] }
      :  {- empty -}        { [] }
      |  varid vars0        { PVar (ann $1) $1 : $2 }

commavars :: { ([S], [Name L]) }
      : varid                 { ([], [$1] ) }
      | varid ',' commavars   { let (ss, ns) = $3 in ($2 : ss, $1 : ns) }

where_decls :: { PatternSynDirection L }
      : 'where' '{' decls '}'       {%  checkExplicitPatSyn $1 $2 $3 $4 }
      | 'where' open decls close    {%  checkExplicitPatSyn $1 $2 $3 $4 }

pattern_synonym_sig :: { Decl L }
      : 'pattern' con_list '::' pstype
            {% do { checkEnabled PatternSynonyms ;
                    let {(qtvs, ps, prov, req_vars, req, ty) = $4} ;
                    let {sig = PatSynSig (nIS $1 <++> ann ty <** [$1] ++ fst $2 ++ [$3] ++ ps)  (snd $2) qtvs prov req_vars req ty} ;
                    return sig } }

pstype :: { (Maybe [TyVarBind L], [S], Maybe (Context L), Maybe [TyVarBind L]
                                      , Maybe (Context L), Type L )}
      :  'forall' ktyvars '.' pstype
            { let (qtvs, ps, prov, req_vars, req, ty) = $4
               in (Just (reverse (fst $2) ++ fromMaybe [] qtvs), ($1 : $3 : ps), prov, req_vars, req, ty) }
      | context context type
            {% do { c1 <- checkContext (Just $1) ;
                    c2 <- checkContext (Just $2) ;
                    t  <- checkType $3 ;
                    return $ (Nothing, [], c1, Nothing, c2, t) }}
      | context 'forall' ktyvars '.' context type
            {% do { c1 <- checkContext (Just $1) ;
                    c2 <- checkContext (Just $5) ;
                    t  <- checkType $6 ;
                    return $ (Nothing, [], c1, Just (reverse (fst $3)), c2, t) }}
      | context type
             {% do { c1 <- checkContext (Just $1);
                     t <- checkType $2;
                     return (Nothing, [], c1, Nothing, Nothing, t) } }
      | type
             {% checkType $1 >>= \t -> return (Nothing, [], Nothing, Nothing, Nothing, t) }

-----------------------------------------------------------------------------
-- | Deriving strategies

deriv_strategy_no_via :: { DerivStrategy L }
      : 'stock'               {% do { checkEnabled DerivingStrategies
                                    ; return (DerivStock (nIS $1)) } }
      | 'anyclass'            {% do { checkEnabled DerivingStrategies
                                    ; checkEnabled DeriveAnyClass
                                    ; return (DerivAnyclass (nIS $1)) } }
      | 'newtype'             {% do { checkEnabled DerivingStrategies
                                    ; checkEnabled GeneralizedNewtypeDeriving
                                    ; return (DerivNewtype (nIS $1)) } }

deriv_strategy_via :: { DerivStrategy L }
      : 'via' truedtype       {% do { checkEnabled DerivingVia
                                    ; return (DerivVia (nIS $1) $2) } }

deriv_standalone_strategy :: { Maybe (DerivStrategy L) }
      : deriv_strategy_no_via { Just $1 }
      | deriv_strategy_via    { Just $1 }
      | {- empty -}           { Nothing }

-----------------------------------------------------------------------------
-- | Miscellaneous (mostly renamings)

modid :: { ModuleName L }
      : CONID                 { let Loc l (ConId  n) = $1 in ModuleName (nIS l) n }
      | QCONID                { let Loc l (QConId n) = $1 in ModuleName (nIS l) (fst n ++ '.':snd n) }

tyconorcls :: { Name L }
      : con                   { $1 }

qtyconorcls :: { QName L }
      : qcon                  { $1 }

tyvar :: { Name L }
      : tyvarid               { $1 }

tyvarid :: { Name L }
      : varid_no_safety       { $1 }

qtyvarop_(ostar) :: { QName L }
qtyvarop_ : '`' tyvar '`'     { UnQual ($1 <^^> $3 <** [$1, srcInfoSpan (ann $2), $3]) $2 }
         | tyvarsym_(ostar)   { UnQual (ann $1) $1 }

tyvarsym_(ostar) :: { Name L }
tyvarsym : VARSYM              { let Loc l (VarSym x) = $1 in Symbol (nIS l) x }
         | '-'                 { Symbol (nIS $1) "-" }
         | ostar                   { Symbol (nIS $1) "*" }

impdeclsblock :: { ([ImportDecl L],[S],L) }
              : '{'  optsemis impdecls optsemis '}'         { let (ids, ss) = $3 in (ids, $1 : reverse $2 ++ ss ++ reverse $4 ++ [$5], $1 <^^> $5) }
              | open optsemis impdecls optsemis close       { let (ids, ss) = $3 in (ids, $1 : reverse $2 ++ ss ++ reverse $4 ++ [$5], $1 <^^> $5) }

-- Exported as partial parsers:

moduletopname :: { (([ModulePragma L], [S], L), Maybe (ModuleName L)) }
              : toppragmas 'module' modid     { ($1, Just $3) }
              | toppragmas {- empty -}        { ($1, Nothing) }

moduletophead :: { (([ModulePragma L], [S], L), Maybe (ModuleHead L)) }
              : toppragmas optmodulehead      { ($1, $2) }

moduletopimps :: { (([ModulePragma L], [S], L), Maybe (ModuleHead L), Maybe ([ImportDecl L],[S],L)) }
              : toppragmas optmodulehead impdeclsblock      { ($1, $2, Just $3) }
              | toppragmas optmodulehead {- empty -}        { ($1, $2, Nothing) }

-----------------------------------------------------------------------------

{

type L = SrcSpanInfo -- just for convenience
type S = SrcSpan

parseError :: Loc Token -> P a
parseError t = fail $ "Parse error: " ++ showToken (unLoc t)

(<>) :: (Annotated a, Annotated b) => a SrcSpanInfo -> b SrcSpanInfo -> SrcSpanInfo
a <> b = ann a <++> ann b

infixl 6 <>

nIS = noInfoSpan
iS = infoSpan

mparseDecl :: P (Decl SrcSpanInfo)
mparseDecl = do
    (is, ds, _, _) <- mparseDeclAux
    when (not $ null is) $
       fail $ "Expected single declaration, found import declaration"
    checkSingleDecl ds

}
