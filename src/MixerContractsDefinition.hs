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

