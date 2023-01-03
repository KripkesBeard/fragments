module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Logic.Prop as Prop (tests)
import qualified Logic.PropModal as PropModal (tests)
import qualified Logic.Quant as Quant (tests)

main :: IO ()
main = do
    --defaultMain Prop.tests
    defaultMain $ testGroup "All tests" [Prop.tests
                                        ,PropModal.tests
                                        ,Quant.tests
                                        ]
