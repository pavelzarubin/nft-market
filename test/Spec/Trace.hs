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
import Minting
import NFTSale
import Plutus.Contract.Test
import Plutus.Trace
import Test.Tasty
import Wallet.Emulator.Wallet

-- Тестовое имя токена
-- Test token name
tn :: TokenName
tn = "ABC"

-- Тестовый "символ валюты"
-- Test "currency symbol"
cur :: CurrencySymbol
cur = "aa"

-- Тестовый NFT
-- Test NFT
nftToken :: AssetClass
nftToken = AssetClass (cur, tn)

---------------------------------------------------------------
-- Тестирование обычной продажи
-- Testing of a normal sale

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
-- Тестирование закрытия продажи
-- Closing the sale test

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
-- Тестирование закрытия несуществующей продажи
-- Testing the closing of a nonexistent sale

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
-- Тестирования старта продажи
-- Sale launch test

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
-- Тестирование перепродажи, одного и того же NFT
-- Resale testing

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
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 1_000_000)
        .&&. walletFundsChange (knownWallet 3) ((lovelaceValueOf (-3_000_000)) <> (assetClassValue nftToken 1))
    )
    testResaleTrace

-----------------------------------------------------------------------------------------------------------
-- Тестирование продажи, после покупки
-- Sales testing, after purchase

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
-- Тестирование закрытия продажи непродавцом
-- Testing the closing of a sale by a non-seller

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

-------------------------------------------------------------
-- Тестирование создания NFT
-- Testing the creation of an NFT

testMintTrace :: EmulatorTrace ()
testMintTrace = do
  h1 <- activateContractWallet (knownWallet 1) mintEndpoints
  callEndpoint @"mint" h1 $
    NFTParams
      { npToken = tn
      -- npAddress = mockWalletAddress (knownWallet 1)
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

runTraceMint :: IO ()
runTraceMint = runEmulatorTraceIO testMintTrace

testMint :: TestTree
testMint =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfClose)
    "test minting"
    ( assertNoFailedTransactions
        .&&. walletFundsChange (knownWallet 1) ((lovelaceValueOf 0) <> (assetClassValue mintedNFT 1))
    )
    testMintTrace

mintedCur :: CurrencySymbol
mintedCur = "606a304282b40f297f9190feff1fb614204646d8d16aee46124a396b"

mintedTn :: TokenName
mintedTn = "ABC"

mintedNFT :: AssetClass
mintedNFT = AssetClass (mintedCur, mintedTn)

------------------------------------------------------------------------
-- Тестирование создания и продажи NFT
-- Testing the creation and sale of NFT

testMintAndSaleTrace :: EmulatorTrace ()
testMintAndSaleTrace = do
  h1 <- activateContractWallet (knownWallet 1) mintEndpoints
  callEndpoint @"mint" h1 $
    NFTParams
      { npToken = tn
      --   npAddress = mockWalletAddress (knownWallet 1)
      }
  void $ waitNSlots 1
  h1s <- activateContractWallet (knownWallet 1) saleEndpoints
  h2 <- activateContractWallet (knownWallet 2) saleEndpoints
  callEndpoint @"start" h1s $
    StartSaleParams
      { sspPrice = 2_000_000,
        sspNFT =
          NFT
            { nftTokenName = mintedTn1,
              nftCurrencySymbol = mintedCur1
            }
      }
  void $ waitNSlots 1
  callEndpoint @"buy" h2 $
    NFT
      { nftTokenName = mintedTn1,
        nftCurrencySymbol = mintedCur1
      }
  void $ waitNSlots 1
  Extras.logInfo @String "end"

testMintAndSale :: TestTree
testMintAndSale =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emuConfBuy)
    "succesfull mint and sale"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf 2_000_000)
        .&&. walletFundsChange (knownWallet 2) ((lovelaceValueOf (-2_000_000)) <> (assetClassValue mintedNFT1 1))
    )
    testMintAndSaleTrace

mintedCur1 :: CurrencySymbol
mintedCur1 = "cd2663da92fa4b61a431137e9655713893664744dfa7a2df0a7032ca"

mintedTn1 :: TokenName
mintedTn1 = "ABC"

mintedNFT1 :: AssetClass
mintedNFT1 = AssetClass (mintedCur1, mintedTn1)
