{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NFTSale (saleEndpoints, NFT (..), StartSaleParams (..)) where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract (ToSchema)
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Semigroup (..), Show (..), String)

minLovelace :: Value
minLovelace = lovelaceValueOf 2_000_000

getLovelaceFromValueOf :: Value -> Integer
getLovelaceFromValueOf = getLovelace . fromValue

data NFT = NFT
  { nftTokenName :: !TokenName,
    nftCurrencySymbol :: !CurrencySymbol
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

instance Eq NFT where
  {-# INLINEABLE (==) #-}
  NFT t1 c1 == NFT t2 c2 = (t1 == t2) && (c1 == c2)

PlutusTx.unstableMakeIsData ''NFT

data NFTSale = NFTSale
  { nsSeller :: !PaymentPubKeyHash,
    nsPrice :: !Integer,
    nsNFT :: !NFT
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''NFTSale

data NFTSaleRedeemer = Close | Buy deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''NFTSaleRedeemer

mkNFTSaleValidator :: NFTSale -> NFTSaleRedeemer -> ScriptContext -> Bool
mkNFTSaleValidator NFTSale {..} red ctx =
  True
    && case red of
      Close -> traceIfFalse "must be signed by seller" signedBySeller
      Buy -> traceIfFalse "low price" checkPrice
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    pkh :: PubKeyHash
    pkh = unPaymentPubKeyHash nsSeller

    signedBySeller :: Bool
    signedBySeller = txSignedBy info pkh

    checkPrice :: Bool
    checkPrice = getLovelaceFromValueOf (valuePaidTo info pkh) >= (nsPrice + 2_000_000)

data Sale

instance Scripts.ValidatorTypes Sale where
  type DatumType Sale = NFTSale
  type RedeemerType Sale = NFTSaleRedeemer

typedValidator :: Scripts.TypedValidator Sale
typedValidator =
  Scripts.mkTypedValidator @Sale
    $$(PlutusTx.compile [||mkNFTSaleValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @NFTSale @NFTSaleRedeemer

saleValidator :: Validator
saleValidator = Scripts.validatorScript typedValidator

saleAddress :: Address
saleAddress = scriptAddress saleValidator

-----------------------------------------------------------------------

data StartSaleParams = StartSaleParams
  { sspNFT :: NFT,
    sspPrice :: Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

startSale :: StartSaleParams -> Contract w NFTSaleSchema Text ()
startSale (StartSaleParams nft price) = do
  pkh <- Contract.ownPaymentPubKeyHash
  let dat =
        NFTSale
          { nsSeller = pkh,
            nsPrice = price,
            nsNFT = nft
          }
      val = Value.singleton (nftCurrencySymbol nft) (nftTokenName nft) 1 <> minLovelace
      tx = (Constraints.mustPayToTheScript dat val)
  ledgerTx <- submitTxConstraints typedValidator tx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Sale of " ++ (show nft) ++ " started for price = " ++ (show price)
  return ()

findNFTSale ::
  NFT ->
  Map.Map TxOutRef ChainIndexTxOut ->
  Maybe (TxOutRef, NFTSale)
findNFTSale nft@NFT {..} utxos = h g
  where
    f = \ch -> (valueOf (_ciTxOutValue ch) nftCurrencySymbol nftTokenName) == 1
    g = Map.toList $ Map.filter f utxos
    h ((oref', ch') : xs) = case _ciTxOutDatum ch' of
      Left _ -> h xs
      Right (Datum d) -> case PlutusTx.fromBuiltinData d :: Maybe NFTSale of
        Nothing -> h xs
        (Just nftSale) -> if nft == (nsNFT nftSale) then Just (oref', nftSale) else Nothing
    h _ = Nothing

closeSale :: NFT -> Contract w NFTSaleSchema Text ()
closeSale nft = do
  utxos <- utxosAt saleAddress
  let tst = findNFTSale nft utxos
  case tst of
    Just (oref, _) -> do
      logInfo @String "Sale founded"
      let lookups =
            Constraints.unspentOutputs utxos
              <> Constraints.otherScript saleValidator
          tx = (Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData Close)
      ledgerTx <- submitTxConstraintsWith @Sale lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String "Sale closed"
    _ -> logError @String $ "Sale not founded. Datums: " ++ (show tst)

buyNFT :: NFT -> Contract w NFTSaleSchema Text ()
buyNFT nft = do
  utxos <- utxosAt saleAddress
  let tst = findNFTSale nft utxos
  case tst of
    Just (oref, NFTSale {..}) -> do
      logInfo @String "Sale founded"
      let lookups =
            Constraints.unspentOutputs utxos
              <> Constraints.otherScript saleValidator
          ada = lovelaceValueOf nsPrice <> minLovelace
          tx =
            (Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData Buy)
              <> (Constraints.mustPayToPubKey nsSeller ada)
      ledgerTx <- submitTxConstraintsWith @Sale lookups tx
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String "NFT bought"
    _ -> logError @String $ "Sale not founded. Datums: " ++ (show tst)

type NFTSaleSchema =
  Endpoint "start" StartSaleParams
    .\/ Endpoint "buy" NFT
    .\/ Endpoint "close" NFT

saleEndpoints :: Contract () NFTSaleSchema Text ()
saleEndpoints = selectList [buy, close, start] >> saleEndpoints
  where
    buy = endpoint @"buy" buyNFT
    close = endpoint @"close" closeSale
    start = endpoint @"start" startSale
