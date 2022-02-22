module Main where

import Spec.Trace
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "nft marketplace tests" [testBuy, testClose, testBadClose, testOnlyStart, testResale, testLateSale, testUnsignedClose]
