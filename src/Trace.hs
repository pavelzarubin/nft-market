{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Trace (runTrace) where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import qualified Data.Map as Map
import Ledger
import Ledger.Ada
import Ledger.Value
import NFTSale (NFT (..), StartSaleParams (..), saleEndpoints)
import Plutus.Contract.Test
import Plutus.Trace

-- Различные эмуляции ситуаций.
-- Different emulations of situations.

testTrace :: EmulatorTrace ()
testTrace = do
  h1 <- activateContractWallet (knownWallet 1) saleEndpoints
  h2 <- activateContractWallet (knownWallet 2) saleEndpoints

  callEndpoint @"start" h1 $
    StartSaleParams
      { sspPrice = 2_000_000,
        sspNFT =
          NFT
            { nftTokenName = tn,
              nftCurrencySymbol = cur
            }
      }
  void $ waitNSlots 1
  callEndpoint @"buy" h2 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

runTrace :: IO ()
runTrace = runEmulatorTraceIO' def emuConfBuy testTrace

emuConfBuy :: EmulatorConfig
emuConfBuy =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 100_000_000 <> assetClassValue nftToken 1),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

emuConfClose :: EmulatorConfig
emuConfClose =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 100_000_000 <> assetClassValue nftToken 1)
             ]
       )

tn :: TokenName
tn = "ABC"

cur :: CurrencySymbol
cur = "aa"

nftToken :: AssetClass
nftToken = AssetClass (cur, tn)

closeTrace :: EmulatorTrace ()
closeTrace = do
  h1 <- activateContractWallet (knownWallet 1) saleEndpoints
  void $
    callEndpoint @"start" h1 $
      StartSaleParams
        { sspPrice = 2_000_000,
          sspNFT =
            NFT
              { nftTokenName = tn,
                nftCurrencySymbol = cur
              }
        }
  void $ waitNSlots 1
  callEndpoint @"close" h1 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

runCloseTrace :: IO ()
runCloseTrace = runEmulatorTraceIO' def emuConfClose closeTrace

onlyStartTrace :: EmulatorTrace ()
onlyStartTrace = do
  h1 <- activateContractWallet (knownWallet 1) saleEndpoints
  callEndpoint @"start" h1 $
    StartSaleParams
      { sspPrice = 2_000_000,
        sspNFT =
          NFT
            { nftTokenName = tn,
              nftCurrencySymbol = cur
            }
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

runOnlyStartTrace :: IO ()
runOnlyStartTrace = runEmulatorTraceIO' def emuConfClose onlyStartTrace

twoSalesTrace :: EmulatorTrace ()
twoSalesTrace = do
  h1 <- activateContractWallet (knownWallet 1) saleEndpoints
  h2 <- activateContractWallet (knownWallet 2) saleEndpoints
  h3 <- activateContractWallet (knownWallet 3) saleEndpoints

  callEndpoint @"start" h1 $
    StartSaleParams
      { sspPrice = 2_000_000,
        sspNFT =
          NFT
            { nftTokenName = tn,
              nftCurrencySymbol = cur
            }
      }
  void $ waitNSlots 1
  callEndpoint @"buy" h2 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  void $ waitNSlots 1
  callEndpoint @"start" h2 $
    StartSaleParams
      { sspPrice = 3_000_000,
        sspNFT =
          NFT
            { nftTokenName = tn,
              nftCurrencySymbol = cur
            }
      }
  void $ waitNSlots 1
  callEndpoint @"buy" h3 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

emuConf2Sales :: EmulatorConfig
emuConf2Sales =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 100_000_000 <> assetClassValue nftToken 1),
               (knownWallet 2, lovelaceValueOf 10_000_000),
               (knownWallet 3, lovelaceValueOf 10_000_000)
             ]
       )

run2Sales :: IO ()
run2Sales = runEmulatorTraceIO' def emuConf2Sales twoSalesTrace
