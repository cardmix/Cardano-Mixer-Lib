{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module MixerAPI where

import           Servant.API

import           MixerProofs.SigmaProtocol

type API = "api" :>
    (
         "keys"                                        :> Get  '[JSON] KeysResponse
    :<|> "withdraw" :> ReqBody '[JSON] WithdrawRequest :> Post '[JSON] ()
    )