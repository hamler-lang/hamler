-----------------------------------------------------------------------------
-- |
-- Module      :  Hamler
-- Copyright   :  (c) Niklas Broberg 2004-2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- An umbrella module for the various functionality
-- of the package. Also provides some convenient
-- functionality for dealing directly with source files.
--
-----------------------------------------------------------------------------

module Hamler (hello, compile) where

import Hamler.Compiler

hello :: IO ()
hello = putStrLn "Hello, Hamler!"

