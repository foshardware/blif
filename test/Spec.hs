
import Control.Monad

import Data.Text hiding (length)
import Data.Text.IO (readFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (readFile)

import Language.BLIF.Builder
import Language.BLIF.Parser (parseBLIF)
import Language.BLIF.Syntax



main :: IO ()
main = do
  putStrLn []
  defaultMain blifs



blifs :: TestTree
blifs = testGroup "BLIF"
  [ testCase "picorv32" $ do

      x <- either (fail . show) pure . parseBLIF =<< picorv32_blif

      assertEqual "command count" 12573 $ sum $ commandLength <$> models x 

      y <- either (fail . show) pure . parseBLIF . toStrict . toLazyText . builderBlif $ x

      assertBool "builder" $ x == y
  ]



picorv32_blif :: IO Text
picorv32_blif = readFile "sample/picorv32.blif"



commandLength :: Model -> Int
commandLength (Model _ _ _ _ xs) = length xs

