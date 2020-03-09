{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler.AST.Comments
-- Copyright   :  (c) JP Moresmau 2015
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module processes comments along with an annotated AST,
-- to be able to associate Haddock comments with the actual item
-- they refer to.
--
-- Example:
--
-- @
-- let
--  parse1Result :: ParseResult (Module SrcSpanInfo,[Comment])
--  parse1Result =
--    parseFileContentsWithComments
--     (defaultParseMode { parseFilename = file })
--      contents
--  withC :: ParseResult (Module (SrcSpanInfo,[Comment]))
--  withC = case parse1Result of
--            ParseOk res         -> ParseOk $ associateHaddock res
--            ParseFailed sloc msg -> ParseFailed sloc msg
-- @
--
-- In this code sample, parse1Result is what you get when you parse a file:
-- a 'Module' annotated wth 'SrcSpanInfo', and a list of comments
-- After passing the result to 'associateHaddock', you get a 'Module'
-- annotated with both a 'SrcSpanInfo' and the list of 'Comment' related to the
-- specific AST node.
--
-----------------------------------------------------------------------------
module Hamler.AST.Comments
    ( associateHaddock
    , Comment(..), UnknownPragma(..)
    ) where

import Hamler.AST.Syntax
import Hamler.AST.SrcLoc

import Data.Char (isSpace)
import Data.Traversable
import Data.Data

-- | A Haskell comment. The 'Bool' is 'True' if the comment is multi-line, i.e. @{- -}@.
data Comment = Comment Bool SrcSpan String
  deriving (Eq,Show,Typeable,Data)

-- | An unknown pragma.
data UnknownPragma = UnknownPragma SrcSpan String
  deriving (Eq,Show,Typeable,Data)


-- | Associates an AST with Source Span Information
-- with relevant Haddock comments
associateHaddock
  ::(Annotated ast,Traversable ast)
  => (ast SrcSpanInfo,[Comment])
  -> ast (SrcSpanInfo,[Comment])
associateHaddock (ast,[]) = fmap (\src->(src,[])) ast
associateHaddock (ast,comments) =
  let
    (ca,assocs1) = mapAccumL associate1 (newAccumulator comments) ast
  in snd $ mapAccumL merge (lastPost ca) assocs1


-- | Merge existing association with post comment associations
merge
  :: [(SrcSpanInfo,[Comment])]
  -> (SrcSpanInfo,[Comment])
  -> ([(SrcSpanInfo,[Comment])], (SrcSpanInfo,[Comment]))
merge [] ret = ([],ret)
merge (x:xs) (src,cmts) =
  if fst x == src
    then (xs,(src,cmts ++ snd x))
    else (x:xs,(src,cmts))


-- | Ensure that if file ends with comment we process it
lastPost :: CommentAccumulator -> [(SrcSpanInfo, [Comment])]
lastPost (CommentAccumulator (Post cmt : rest) past assocs) =
  let (toMerge, _) = span isNone rest
      psrc = matchPreviousSrc past
  in (assocs ++ [(psrc, cmt : map hcComment toMerge)])
lastPost (CommentAccumulator _ _ assocs) = assocs


-- | Accumulate comments mappings, either directly with the source
-- or in another association list for later processing
associate1
  :: CommentAccumulator
  -> SrcSpanInfo
  -> (CommentAccumulator,(SrcSpanInfo,[Comment]))
associate1 ca@(CommentAccumulator [] _ _) src = (ca,(src,[]))
associate1 (CommentAccumulator (hc@(Pre cmt):rest) _ assocs) src =
  if isBefore hc src
    then
      let (toMerge,next) = getToMerge src rest
          newAssoc = (src,cmt : map hcComment toMerge)
      in (CommentAccumulator next [] assocs,newAssoc)
    else (CommentAccumulator (hc:rest) [] assocs,(src,[]))
associate1 (CommentAccumulator (hc@(Post cmt):rest) past assocs) src =
  if isBefore hc src
    then
      let (toMerge,next) = getToMerge src rest
          newAssocs =
            if null past
              then assocs
              else assocs++[(matchPreviousSrc past,cmt : map hcComment toMerge)]
      in associate1 (CommentAccumulator next [] newAssocs) src
    else (CommentAccumulator (hc:rest) (src:past) assocs,(src,[]))
associate1 (CommentAccumulator (_:rest) past assocs) src =
  (CommentAccumulator rest (src:past) assocs,(src,[]))


-- | The comment accumulator
data CommentAccumulator = CommentAccumulator
  [HaddockComment]          -- The Haddock comments to process
  [SrcSpanInfo]             -- The past src infos to resolve post comments
  [(SrcSpanInfo,[Comment])] -- The additional associations between src and comments


-- | Create a new accumulator
newAccumulator :: [Comment] -> CommentAccumulator
newAccumulator comments = CommentAccumulator (commentsToHaddock comments) [] []

-- | Get comments to merge
getToMerge
  :: SrcSpanInfo                         -- ^ Stop before src
  -> [HaddockComment]                    -- ^ All remaining comments
  -> ([HaddockComment],[HaddockComment]) -- ^ Comments to merge, left overs
getToMerge src = span (\hc-> isNone hc && isBefore hc src)


-- | Get the biggest src that ends where the first one does
matchPreviousSrc :: [SrcSpanInfo] -> SrcSpanInfo
matchPreviousSrc [] =
  error "Hamler.AST.Comments.matchPreviousSrc: empty list"
matchPreviousSrc srcs =
  let end = srcSpanEnd $ srcInfoSpan $ head srcs
  in last $ filter ((end ==) . srcSpanEnd . srcInfoSpan) srcs

-- | Is a Haddock comment before a given location
isBefore :: HaddockComment -> SrcSpanInfo -> Bool
isBefore hc src=
  let
    (Comment _ csrc _) = hcComment hc
  in csrc < srcInfoSpan src

-- | Represents a Haddock Comment
data HaddockComment =
  -- | Comment before declaration
  Pre
   {
     hcComment::Comment
   }
  -- | Comment after declaration
  | Post  {
     hcComment::Comment
    }
  -- | Non Haddock comment
  | None  {
     hcComment::Comment
    }

-- | Is a comment not haddock?
isNone :: HaddockComment -> Bool
isNone (None _) = True
isNone _ = False


-- | Comments to Haddock Comments
commentsToHaddock :: [Comment] -> [HaddockComment]
commentsToHaddock = map commentToHaddock

-- | Comment to Haddock Comment
commentToHaddock :: Comment -> HaddockComment
commentToHaddock c@(Comment _ _ txt) =
  case dropWhile isSpace txt of
    ('|':_) -> Pre c
    ('^':_) -> Post c
    _       -> None c
