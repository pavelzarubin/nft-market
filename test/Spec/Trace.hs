{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Trace where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import qualified Data.Map as Map
import Ledger.Ada
import Ledger.Value
import NFTSale
import Plutus.Contract.Test
import Plutus.Trace
import Test.Tasty

tn :: TokenName
tn = "ABC"

cur :: CurrencySymbol
cur = "aa"

nftToken :: AssetClass
nftToken = AssetClass (cur, tn)

---------------------------------------------------------------
testTraceBuy :: EmulatorTrace ()
testTraceBuy = do
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

emuConfBuy :: EmulatorConfig
emuConfBuy =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 100_000_000 <> assetClassValue nftToken 1),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

testBuy :: TestTree
testBuy =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfBuy)
    "succesfull sale"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf 2_000_000) <> (assetClassValue nftToken (-1)))
        .&&. walletFundsChange (knownWallet 2) ((lovelaceValueOf (-2_000_000)) <> (assetClassValue nftToken 1))
    )
    testTraceBuy

-------------------------------------------------------------------------------

testCloseTrace :: EmulatorTrace ()
testCloseTrace = do
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
  callEndpoint @"close" h1 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

emuConfClose :: EmulatorConfig
emuConfClose =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 100_000_000 <> assetClassValue nftToken 1)
             ]
       )

testClose :: TestTree
testClose =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfBuy)
    "succesfull close"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf 0) <> (assetClassValue nftToken 0))
    )
    testCloseTrace

-------------------------------------------------------------------------------------------------------
testBadCloseTrace :: EmulatorTrace ()
testBadCloseTrace = do
  h1 <- activateContractWallet (knownWallet 1) saleEndpoints
  callEndpoint @"close" h1 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  --void $ waitNSlots 1
  Extras.logInfo @String "end"

testBadClose :: TestTree
testBadClose =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfBuy)
    "bad close"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf 0) <> (assetClassValue nftToken 0))
        .&&. (assertNotDone saleEndpoints (walletInstanceTag (knownWallet 1)) "sale not closing, because sale not exist")
    )
    testBadCloseTrace

-------------------------------------------------------------------------------------------------------
testOnlyStartTrace :: EmulatorTrace ()
testOnlyStartTrace = do
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

testOnlyStart :: TestTree
testOnlyStart =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfBuy)
    "succesfull start"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf (-2_000_000)) <> (assetClassValue nftToken (-1)))
    )
    testOnlyStartTrace

------------------------------------------------------------------------------------------------------

testResaleTrace :: EmulatorTrace ()
testResaleTrace = do
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

testResale :: TestTree
testResale =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConf2Sales)
    "succesfull resale"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf 2_000_000) <> (assetClassValue nftToken (-1)))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf (1_000_000))
        .&&. walletFundsChange (knownWallet 3) ((lovelaceValueOf (-3_000_000)) <> (assetClassValue nftToken 1))
    )
    testResaleTrace

-----------------------------------------------------------------------------------------------------------

testLateSaleTrace :: EmulatorTrace ()
testLateSaleTrace = do
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
  callEndpoint @"buy" h3 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

testLateSale :: TestTree
testLateSale =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConf2Sales)
    "bad sale, because nft already sold"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf 2_000_000) <> (assetClassValue nftToken (-1)))
        .&&. walletFundsChange (knownWallet 2) ((lovelaceValueOf (-2_000_000)) <> (assetClassValue nftToken 1))
        .&&. walletFundsChange (knownWallet 3) (lovelaceValueOf 0)
        .&&. assertNotDone saleEndpoints (walletInstanceTag (knownWallet 3)) "nft already sold"
    )
    testLateSaleTrace

--------------------------------------------------------------------------------------------------

testUnsignedCloseTrace :: EmulatorTrace ()
testUnsignedCloseTrace = do
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
  callEndpoint @"close" h2 $
    NFT
      { nftTokenName = tn,
        nftCurrencySymbol = cur
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

testUnsignedClose :: TestTree
testUnsignedClose =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfBuy)
    "bad close, because not seller try close sale"
    ( walletFundsChange (knownWallet 1) ((lovelaceValueOf (-2_000_000)) <> (assetClassValue nftToken (-1)))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
        .&&. (Plutus.Contract.Test.not assertNoFailedTransactions)
    )
    testUnsignedCloseTrace
