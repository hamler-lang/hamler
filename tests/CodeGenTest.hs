module CodeGenTest where

import           Data.Map                         as M
import           Data.Text                        (Text, unpack)
import qualified Data.Text.IO                     as TIO
import           Language.CodeGen
import           Language.CoreErlang.Pretty
import           Language.CoreErlang.Syntax       as E
import           Language.CoreErlang.Pretty       as P
import           Language.PureScript.AST          as A
import qualified Language.PureScript.AST.Literals as L
import           Language.PureScript.CST.Convert
import           Language.PureScript.CST.Parser
import           Language.PureScript.Names
import           Language.PureScript.PSString
import           Prelude


t :: IO ()
t = do
  res <- TIO.readFile "tests/data/Main.purs"
  let Right r = parse res
  -- print r
  putStr $  P.prettyPrint $ aM2Em $ convertModule "test" r



