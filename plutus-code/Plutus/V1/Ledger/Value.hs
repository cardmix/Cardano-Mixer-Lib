{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}

module Plutus.V1.Ledger.Value where

import           Data.Aeson     (FromJSON(..), ToJSON(..), FromJSONKey, ToJSONKey)
import           Data.Map       (Map)
import           GHC.Generics   (Generic)
import           Prelude        

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

newtype Value = Value { getValue :: Map CurrencySymbol (Map TokenName Integer) }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
