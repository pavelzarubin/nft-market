{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NFTSale where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
--import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema)
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Eq, Semigroup (..), Show (..), String, last)

data NFT = NFT
  { nftTokenName :: !TokenName,
    nftCurrencySymbol :: !CurrencySymbol
  }
  deriving (Show, Generic, FromJSON, ToJSON)

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
mkNFTSaleValidator saleParams red ctx = True
