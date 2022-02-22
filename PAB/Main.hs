{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (void)
import Control.Monad.Freer (interpret)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Default (def)
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)
import Ledger.Value (CurrencySymbol, TokenName)
import Minting
import NFTSale
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Prettyprinter (Pretty (..), viaShow)
import Wallet.Emulator.Wallet (knownWallet)

data SaleContracts = StartSale StartSaleParams | CloseSale NFT | BuyNFT NFT | MintNFT NFTParams
  deriving stock (Show, Eq, Generic)
  deriving anyclass (OpenApi.ToSchema, ToJSON, FromJSON)

instance Pretty SaleContracts where
  pretty = viaShow

instance Builtin.HasDefinitions SaleContracts where
  getDefinitions = []
  getSchema = \case
    StartSale _ -> Builtin.endpointsToSchemas @NFTSaleSchema
    CloseSale _ -> Builtin.endpointsToSchemas @NFTSaleSchema
    BuyNFT _ -> Builtin.endpointsToSchemas @NFTSaleSchema
    MintNFT _ -> Builtin.endpointsToSchemas @MintSchema
  getContract = \case
    StartSale ssp -> SomeBuiltin $ startSale @() ssp
    CloseSale nft -> SomeBuiltin $ closeSale @() nft
    BuyNFT nft -> SomeBuiltin $ buyNFT @() nft
    MintNFT nftp -> SomeBuiltin $ mint @() nftp

handlers :: SimulatorEffectHandlers (Builtin SaleContracts)
handlers =
  Simulator.mkSimulatorHandlers def def $
    interpret (contractHandler Builtin.handleBuiltin)

tn :: TokenName
tn = "ABC"

mintp :: NFTParams
mintp = NFTParams tn

cur :: CurrencySymbol
cur = "fc2b07f4236535d4e6b693c5981e69f844c7dcaf505fcd92f3d7fee4"

nft :: NFT
nft =
  NFT
    { nftTokenName = tn,
      nftCurrencySymbol = cur
    }

ss :: StartSaleParams
ss =
  StartSaleParams
    { sspNFT = nft,
      sspPrice = 30_000_000_000
    }

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin SaleContracts) "Simulation started"
    shutdown <- PAB.Server.startServerDebug

    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2

    void $ Simulator.activateContract wallet1 $ MintNFT mintp

    Simulator.waitNSlots 1

    void $ Simulator.activateContract wallet1 $ StartSale ss

    Simulator.waitNSlots 1

    void $ Simulator.activateContract wallet2 $ BuyNFT nft

    Simulator.waitNSlots 1

    Simulator.logString @(Builtin SaleContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin SaleContracts) b

    shutdown
