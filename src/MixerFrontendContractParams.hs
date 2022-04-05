{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}


module MixerFrontendContractParams where

import           Data.Aeson                          (FromJSON(..), ToJSON(..), FromJSONKey(..), ToJSONKey(..))
import           Data.Map
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Prelude

import           Crypto

---------------------------------------- Types for API calls to the PAB -------------------------------------

-- Parameters for the "deposit" endpoint
data DepositParams = DepositParams
    {
        dpAddress        :: !Text,
        dpValue          :: !Value,
        dpLeaf           :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Parameters for the "withdraw" endpoint
data WithdrawParams = WithdrawParams
    {
        wpValue         :: !Value,
        wpDepositNum    :: !(Integer, Integer),
        wpAddress       :: !Text,
        wpPublicInputs  :: ![Fr],
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Wallet = Wallet { getWalletId :: String }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Value = Value { getValue :: Map CurrencySymbol (Map TokenName Integer) }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: String }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance Ord CurrencySymbol where
    compare a b = compare (read $ "0x" ++ unCurrencySymbol a :: Integer) (read $ "0x" ++ unCurrencySymbol b)

newtype TokenName = TokenName { unTokenName :: String }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance Ord TokenName where
    compare a b = compare (read $ "0x" ++ unTokenName a :: Integer) (read $ "0x" ++ unTokenName b)

newtype PaymentPubKeyHash = PaymentPubKeyHash { unPaymentPubKeyHash :: PubKeyHash }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PubKeyHash = PubKeyHash { getPubKeyHash :: String }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
