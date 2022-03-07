{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}


module Requests (activateRequest, endpointRequest, statusRequest, awaitStatusUpdate, stopRequest) where

import           Control.Concurrent       (threadDelay)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Aeson               
import qualified Data.OpenApi             as OpenApi
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text, pack)
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)
import           Network.HTTP.Req
import           Prelude

import           MixerFrontendContracts   hiding (Value)


------------------------------- API Requests -------------------------------------

-- Activate a contract for a given wallet
activateRequest :: Text -> MixerFrontendContracts -> Maybe Wallet -> IO UUID
activateRequest ip x w = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http ip /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ ContractActivationArgs x w)
        (Proxy :: Proxy (JsonResponse ContractInstanceId))
        (port 9080)
    return $ unContractInstanceId $ responseBody v

-- Call an endpoint
endpointRequest :: (ToJSON p, Show p) => Text -> Text -> UUID -> p -> IO ()
endpointRequest ip endp uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http ip /: "api"  /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: endp)
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed: " ++ show x
        else "Error processing request!"

statusRequest :: (FromJSON p) => Text -> UUID -> IO (Maybe p)
statusRequest ip uuid = runReq defaultHttpConfig $ do
    v <- req
        GET
        (http ip /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse ContractState))
        (port 9080)
    let x = fromJSON $ observableState $ cicCurrentState $ responseBody v
    case x of
        Success tt -> return (Just tt)
        Error _    -> return Nothing

awaitStatusUpdate :: FromJSON a => Text -> UUID -> IO a
awaitStatusUpdate ip cid = do
    resp <- statusRequest ip cid
    case resp of
        Just state -> return state
        Nothing    -> do
                    threadDelay 1_000_000
                    awaitStatusUpdate ip cid

stopRequest :: Text -> UUID -> IO ()
stopRequest ip uuid = runReq defaultHttpConfig $ do
    v <- req
        PUT
        (http ip /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "stop")
        NoReqBody
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed!"
        else "Error processing request!"

---------------------- Requests and responses -----------------------------

newtype ContractInstanceId = ContractInstanceId { unContractInstanceId :: UUID }
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (FromJSONKey, ToJSONKey)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

-- | Data needed to start a new instance of a contract.
data ContractActivationArgs t = ContractActivationArgs
    {
        caID     :: t,           -- ^ ID of the contract
        caWallet :: Maybe Wallet -- ^ Wallet that should be used for this instance, `knownWallet 1` is used in the Nothing case.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype ContractState = ContractState { cicCurrentState :: PartiallyDecodedResponse }

instance FromJSON ContractState where
    parseJSON = withObject "ContractState" $ \v -> ContractState <$> v .: "cicCurrentState"

newtype PartiallyDecodedResponse = PartiallyDecodedResponse { observableState :: Value }

instance FromJSON PartiallyDecodedResponse where
    parseJSON = withObject "PartiallyDecodedResponse" $ \v -> PartiallyDecodedResponse <$> v .: "observableState"

