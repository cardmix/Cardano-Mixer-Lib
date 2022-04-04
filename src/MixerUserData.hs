{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module MixerUserData where

import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Text                        (Text, unpack)
import           GHC.Generics                     (Generic)
import           PlutusTx.Prelude                 hiding ((<$>), (<*>))
import           Prelude                          (IO, Show(..), (<$>), (<*>), read)

import           Crypto

data DepositSecret = DepositSecret
    {
        getR1 :: Fr,
        getR2 :: Fr
    } deriving (Generic, FromJSON, ToJSON)

instance Show DepositSecret where
    show (DepositSecret r1 r2) = show $ fromZp r1 * fieldPrime R + fromZp r2

readDepositSecret :: Text -> DepositSecret
readDepositSecret str = DepositSecret (toZp r1) (toZp r2)
    where
        n = read $ unpack str :: Integer
        (r1, r2) = divMod n (fieldPrime R)

generateDepositSecret :: IO DepositSecret
generateDepositSecret = DepositSecret <$> generateFr <*> generateFr

data ShieldedAccountSecret = ShieldedAccountSecret
    {
        getV1 :: Fr,
        getV2 :: Fr,
        getV3 :: Fr
    } deriving (Show, Generic, FromJSON, ToJSON)

generateShieldedAccountSecret :: IO ShieldedAccountSecret
generateShieldedAccountSecret = ShieldedAccountSecret <$> generateFr <*> generateFr <*> generateFr











