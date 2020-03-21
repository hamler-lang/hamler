-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.CodeGen.CoreErl.Pretty
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alex Kropivny <alex.kropivny@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty printer for CoreErlang.
--
-----------------------------------------------------------------------------

module Hamler.CodeGen.CoreErl.Pretty (
    -- * Pretty printing
    Pretty,
    prettyPrintStyleMode, prettyPrintWithMode, prettyPrint,
    -- * Pretty-printing styles (from -- "Text.PrettyPrint.HughesPJ")
    P.Style(..), P.style, P.Mode(..),
    -- * CoreErlang formatting modes
    PPMode(..), Indent, PPLayout(..), defaultMode
  ) where

import Prelude hiding ((<>))
import qualified Text.PrettyPrint as P

import Hamler.CodeGen.CoreErl.Syntax

infixl 5 $$$

-----------------------------------------------------------------------------

-- | Varieties of layout we can use.
data PPLayout = PPDefault   -- ^ classical layout
              | PPNoLayout  -- ^ everything on a single line
  deriving Eq

type Indent = Int

-- | Pretty-printing parameters.
data PPMode = PPMode {
                    altIndent :: Indent,    -- ^ indentation of the alternatives
                                            -- in a @case@ expression
                    caseIndent :: Indent,   -- ^ indentation of the declarations
                                            -- in a @case@ expression
                    fundefIndent :: Indent, -- ^ indentation of the declarations
                                            -- in a function definition
                    lambdaIndent :: Indent, -- ^ indentation of the declarations
                                            -- in a @lambda@ expression
                    letIndent :: Indent,    -- ^ indentation of the declarations
                                            -- in a @let@ expression
                    letrecIndent :: Indent, -- ^ indentation of the declarations
                                            -- in a @letrec@ expression
                    onsideIndent :: Indent, -- ^ indentation added for continuation
                                            -- lines that would otherwise be offside
                    layout :: PPLayout      -- ^ Pretty-printing style to use
                  }

-- | The default mode: pretty-print using sensible defaults.
defaultMode :: PPMode
defaultMode = PPMode {
                altIndent = 4,
                caseIndent = 4,
                fundefIndent = 4,
                lambdaIndent = 4,
                letIndent = 4,
                letrecIndent = 4,
                onsideIndent = 4,
                layout = PPDefault
              }

-- | Pretty printing monad
newtype DocM s a = DocM (s -> a)

instance Functor (DocM s) where
         fmap f xs = do x <- xs; return (f x)

instance Applicative (DocM s) where
         pure        = return
         (<*>) m1 m2 = do x1 <- m1; x2 <- m2; return (x1 x2)

instance Monad (DocM s) where
         (>>=) = thenDocM
         (>>) = then_DocM
         return = retDocM

{-# INLINE thenDocM #-}
{-# INLINE then_DocM #-}
{-# INLINE retDocM #-}
{-# INLINE unDocM #-}
{-# INLINE getPPEnv #-}

thenDocM :: DocM s a -> (a -> DocM s b) -> DocM s b
thenDocM m k = DocM $ (\s -> case unDocM m $ s of a -> unDocM (k a) $ s)

then_DocM :: DocM s a -> DocM s b -> DocM s b
then_DocM m k = DocM $ (\s -> case unDocM m $ s of _ -> unDocM k $ s)

retDocM :: a -> DocM s a
retDocM a = DocM (\_s -> a)

unDocM :: DocM s a -> (s -> a)
unDocM (DocM f) = f

-- all this extra stuff, just for this one function.
getPPEnv :: DocM s s
getPPEnv = DocM id

-- | The document type produced by these pretty printers uses a 'PPMode'
-- environment.
type Doc = DocM PPMode P.Doc

-- | Things that can be pretty-printed, including all the syntactic objects
-- in "Language.CoreErlang.Syntax".
class Pretty a where
        -- | Pretty-print something in isolation.
        pretty :: a -> Doc
        -- | Pretty-print something in a precedence context.
        prettyPrec :: Int -> a -> Doc
        pretty = prettyPrec 0
        prettyPrec _ = pretty

-- The pretty printing combinators

empty :: Doc
empty = return P.empty

nest :: Int -> Doc -> Doc
nest i m = m >>= return . P.nest i

-- Literals
text, ptext :: String -> Doc
text = return . P.text
ptext = return . P.text

char :: Char -> Doc
char = return . P.char

int :: Int -> Doc
int = return . P.int

integer :: Integer -> Doc
integer = return . P.integer

float :: Float -> Doc
float = return . P.float

double :: Double -> Doc
double = return . P.double

-- Simple Combining Forms
parens, brackets, braces, quotes, doubleQuotes :: Doc -> Doc
parens d = d >>= return . P.parens
brackets d = d >>= return . P.brackets
braces d = d >>= return . P.braces
quotes d = d >>= return . P.quotes
doubleQuotes d = d >>= return . P.doubleQuotes

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

-- Constants
semi, comma, colon, space, equals :: Doc
semi = return P.semi
comma = return P.comma
colon = return P.colon
space = return P.space
equals = return P.equals

lparen, rparen, lbrack, rbrack, lbrace, rbrace :: Doc
lparen = return P.lparen
rparen = return P.rparen
lbrack = return P.lbrack
rbrack = return P.rbrack
lbrace = return P.lbrace
rbrace = return P.rbrace

-- Combinators

(<>),(<+>),($$),($+$) :: Doc -> Doc -> Doc
aM <> bM = do { a <- aM; b <- bM; return $ a P.<> b}
aM <+> bM = do { a <- aM; b <- bM; return $ a P.<+> b}
aM $$ bM = do { a <- aM; b <- bM; return $ a P.$$ b}
aM $+$ bM = do { a <- aM; b <- bM; return $ a P.$+$ b}

hcat, hsep, vcat, sep, cat, fsep, fcat :: [Doc] -> Doc
hcat dl = sequence dl >>= return . P.hcat
hsep dl = sequence dl >>= return . P.hsep
vcat dl = sequence dl >>= return . P.vcat
sep dl = sequence dl >>= return . P.sep
cat dl = sequence dl >>= return . P.cat
fsep dl = sequence dl >>= return . P.fsep
fcat dl = sequence dl >>= return . P.fcat

-- Some More

hang :: Doc -> Int -> Doc -> Doc
hang dM i rM = do { d <- dM; r <- rM; return $ P.hang d i r }

-- Yuk, had to cut-n-paste this one from Pretty.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (d1:ds) = go d1 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

-- | render the document with a given style and mode.
renderStyleMode :: P.Style -> PPMode -> Doc -> String
renderStyleMode ppStyle ppMode d = P.renderStyle ppStyle . unDocM d $ ppMode

-- | render the document with a given mode.
renderWithMode :: PPMode -> Doc -> String
renderWithMode = renderStyleMode P.style

-- | render the document with defaultMode
render :: Doc -> String
render = renderWithMode defaultMode

-- | pretty-print with a given style and mode.
prettyPrintStyleMode :: Pretty a => P.Style -> PPMode -> a -> String
prettyPrintStyleMode ppStyle ppMode = renderStyleMode ppStyle ppMode . pretty

-- | pretty-print with the default style and a given mode.
prettyPrintWithMode :: Pretty a => PPMode -> a -> String
prettyPrintWithMode = prettyPrintStyleMode P.style

-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> String
prettyPrint = prettyPrintWithMode defaultMode

fullRenderWithMode :: PPMode -> P.Mode -> Int -> Float ->
                      (P.TextDetails -> a -> a) -> a -> Doc -> a
fullRenderWithMode ppMode m i f fn e mD =
                   P.fullRender m i f fn e $ (unDocM mD) ppMode

fullRender :: P.Mode -> Int -> Float -> (P.TextDetails -> a -> a)
              -> a -> Doc -> a
fullRender = fullRenderWithMode defaultMode

-------------------------  Pretty-Print a Module --------------------
instance Pretty EModule where
        pretty (EModule m exports attrs fundefs) =
                topLevel (ppModuleHeader m exports attrs)
                         (map pretty fundefs)

--------------------------  Module Header ------------------------------
ppModuleHeader :: EAtom -> [EFunction] -> [(EAtom,EConst)] -> Doc
ppModuleHeader m exports attrs = myFsep [
        text "module" <+> pretty m <+> (bracketList $ map pretty exports),
        text "attributes" <+> bracketList (map ppAssign attrs)]

instance Pretty EFunction where
        pretty (EFunction (name,arity)) =
                pretty name <> char '/' <> integer arity

instance Pretty EConst where
        pretty (ECLit l) = pretty l
        pretty (ECTuple l) = ppTuple l
        pretty (ECList l) = pretty l

-------------------------  Declarations ------------------------------
instance Pretty EFunDef where
        pretty (EFunDef function exp) = (pretty function <+> char '=') $$$
                                           ppBody fundefIndent [pretty exp]

------------------------- Expressions -------------------------
instance Pretty ELiteral where
        pretty (ELChar c) = char c
        pretty (ELString  s) = text (show s)
        pretty (ELInt i) = integer i
        pretty (ELFloat f) = double f
        pretty (ELAtom a) = pretty a
        pretty ELNil = bracketList [empty]

instance Pretty EAtom where
        pretty (EAtom a) = char '\'' <> text a <> char '\''

instance Pretty EExps where
        pretty (EExp e) = pretty e
        pretty (EExps (EConstr e)) = angleList (map pretty e)
        pretty (EExps (EAnn e cs)) = parens (angleList (map pretty e)
                                                    $$$ ppAnn cs)

instance Pretty EExp where
        pretty (EVar v) = text v
        pretty (ELit l) = pretty l
        pretty (EFun f) = pretty f
        pretty (EApp e exps) = text "apply" <+>
                                   pretty e <> parenList (map pretty exps)
        pretty (EModCall (e1,e2) exps) = sep [text "call" <+>
                                              pretty e1 <> char ':' <> pretty e2,
                                              parenList (map pretty exps)]
        pretty (ELambda vars e) = sep [text "fun" <> parenList (map text vars) <+> text "->",
                                               ppBody lambdaIndent [pretty e]]
        pretty (ESeq e1 e2) = sep [text "do", pretty e1, pretty e2]
        pretty (ELet (vars,e1) e2) = text "let" <+>
                                         angleList (map text vars) <+>
                                         char '=' <+> pretty e1
                                         $$$ text "in" <+> pretty e2
        pretty (ELetRec fundefs e) = sep [text "letrec" <+>
                                              ppBody letrecIndent (map pretty fundefs),
                                              text "in", pretty e]
        pretty (ECase e alts) = sep [text "case", pretty e, text "of"]
                                    $$$ ppBody caseIndent (map pretty alts)
                                    $$$ text "end"
        pretty (ETuple exps) = braceList $ map pretty exps
        pretty (EList l) = pretty l
        pretty (EOp a exps) = text "primop" <+> pretty a <> parenList (map pretty exps)
        pretty (EBinary bs) = char '#' <> braceList (map pretty bs) <> char '#'
        pretty (ETry e (vars1,exps1) (vars2,exps2)) = text "try"
                                                      $$$ ppBody caseIndent [pretty e]
                                                      $$$ text "of" <+> angleList (map text vars1) <+> text "->"
                                                      $$$ ppBody altIndent [pretty exps1]
                                                      $$$ text "catch" <+> angleList (map text vars2) <+> text "->"
                                                      $$$ ppBody altIndent [pretty exps2]
        pretty (ERec alts tout) = text "receive"
                                  $$$ ppBody caseIndent (map pretty alts)
                                  $$$ text "after"
                                  $$$ ppBody caseIndent [pretty tout]
        pretty (ECatch e) = sep [text "catch", pretty e]

instance Pretty a => Pretty (EList a) where
        pretty (EL l) = bracketList $ map pretty l
        pretty (ELL h t) = brackets . hcat $ punctuate comma (map pretty h) ++
                                            [char '|' <> pretty t]
instance Pretty EAlt where
        pretty (EAlt pats guard exps) =
                myFsep [pretty pats, pretty guard <+> text "->"]
                $$$ ppBody altIndent [pretty exps]

instance Pretty EPats where
        pretty (EPat p) = pretty p
        pretty (EPats p) = angleList (map pretty p)

instance Pretty EPat where
        pretty (EPVar v) = text v
        pretty (EPLit l) = pretty l
        pretty (EPTuple p) = braceList $ map pretty p
        pretty (EPList l) = pretty l
        pretty (EPBinary bs) = char '#' <> braceList (map pretty bs) <> char '#'
        pretty (EPAlias a) = pretty a

instance Pretty EAlias where
        pretty (EAlias v p) = ppAssign (EVar v,p) -- FIXME: hack!

instance Pretty EGuard where
        pretty (EGuard e) = text "when" <+> pretty e

instance Pretty ETimeOut where
        pretty (ETimeOut e1 e2) = pretty e1 <+> text "->"
                                  $$$ ppBody altIndent [pretty e2]

instance Pretty a => Pretty (EBitString a) where
        pretty (EBitString e es) = text "#<" <> pretty e <> char '>' <> parenList (map pretty es)

----------------------- Annotations ------------------------
instance Pretty a => Pretty (EAnn a) where
        pretty (EConstr a) = pretty a
        pretty (EAnn a cs) = parens (pretty a $$$ ppAnn cs)


------------------------- pp utils -------------------------
angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

angleList :: [Doc] -> Doc
angleList = angles . myFsepSimple . punctuate comma

braceList :: [Doc] -> Doc
braceList = braces . myFsepSimple . punctuate comma

bracketList :: [Doc] -> Doc
bracketList = brackets . myFsepSimple . punctuate comma

parenList :: [Doc] -> Doc
parenList = parens . myFsepSimple . punctuate comma

-- | Monadic PP Combinators -- these examine the env
topLevel :: Doc -> [Doc] -> Doc
topLevel header dl = do e <- fmap layout getPPEnv
                        let s = case e of
                                    PPDefault -> header $$ vcat dl
                                    PPNoLayout -> header <+> hsep dl
                        s $$$ text "end"

ppAssign :: (Pretty a,Pretty b) => (a,b) -> Doc
ppAssign (a,b) = pretty a <+> char '=' <+> pretty b

ppTuple :: Pretty a => [a] -> Doc
ppTuple t = braceList (map pretty t)

ppBody :: (PPMode -> Int) -> [Doc] -> Doc
ppBody f dl = do e <- fmap layout getPPEnv
                 i <- fmap f getPPEnv
                 case e of
                     PPDefault -> nest i . vcat $ dl
                     _         -> hsep dl

($$$) :: Doc -> Doc -> Doc
a $$$ b = layoutChoice (a $$) (a <+>) b

myFsepSimple :: [Doc] -> Doc
myFsepSimple = layoutChoice fsep hsep

-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
myFsep :: [Doc] -> Doc
myFsep = layoutChoice fsep' hsep
    where   fsep' [] = empty
            fsep' (d:ds) = do
                    e <- getPPEnv
                    let n = onsideIndent e
                    nest n (fsep (nest (-n) d:ds))

layoutChoice :: (a -> Doc) -> (a -> Doc) -> a -> Doc
layoutChoice a b dl = do e <- getPPEnv
                         if layout e == PPDefault
                            then a dl
                            else b dl

ppAnn :: (Pretty a) => [a] -> Doc
ppAnn cs = text "-|" <+> bracketList (map pretty cs)
