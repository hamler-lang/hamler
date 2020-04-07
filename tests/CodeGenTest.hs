module CodeGenTest where

import           Data.Map                         as M
import           Data.Text                        (Text, unpack,pack)
import qualified Data.Text.IO                     as TIO
import           Language.Refactor
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
import           Shelly



t :: IO ()
t = do
  res <- TIO.readFile "tests/data/Main.purs"
  let Right r = parse res
      cr = convertModule "test" r
      ((end,gs),log) = runTranslate $ moduleToMod cr
  case end of
    Left e -> print e
    Right e -> do
      -- print $  cr
      putStr $  P.prettyPrint $ e
      TIO.writeFile "tests/data/main.core"
        (pack $ P.prettyPrint e)
  -- r <- shelly $ run "erlc" ["+time","tests/data/main.core"]
      print "finish erlc"
  -- print gs
  print log

