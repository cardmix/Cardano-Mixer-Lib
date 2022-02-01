{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}


module MixerContractsDefinition where

import           Data.Aeson                          (FromJSON(..), ToJSON(..))
import qualified Data.OpenApi
import           GHC.Generics                        (Generic)
import           Prelude                             (Eq, Ord, Show)

data MixerContractsDefinition = MintAdminKey | UseMixer | MixerStateQuery | ConnectToPAB | RetrieveTimeLocked
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
    deriving anyclass (Data.OpenApi.ToSchema)

---------------------------------------- Types for API calls to the PAB -------------------------------------

-- Parameters for the "deposit" endpoint
data DepositParams = DepositParams
    {
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
        wpPKH           :: !PaymentPubKeyHash,
        wpPublicInputs  :: ![Fr],
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Wallet = Wallet { getWalletId :: String }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Value = Value { getValue :: Map CurrencySymbol (Map TokenName Integer) }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: String }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSONKey, FromJSONKey)

newtype TokenName = TokenName { unTokenName :: String }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype PaymentPubKeyHash = PaymentPubKeyHash { unPaymentPubKeyHash :: PubKeyHash }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype PubKeyHash = PubKeyHash { getPubKeyHash :: String }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)