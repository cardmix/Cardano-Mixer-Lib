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

module MixerProofs.SigmaProtocol.API where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           PlutusTx.Prelude          hiding (Eq, elem, mapM, (<$>), fmap)
import           Prelude                   (IO, Eq, Show, (<$>), elem, fmap)

import           Crypto
import           MixerProofs.SigmaProtocol.OffChain
import           MixerProofs.SigmaProtocol.Types


data MixerInstanceFrontend = MixerInstanceFrontend
  {
    mifGenerators             :: SigmaProtocolGenerators,
    mifCurrentDepositAddress  :: Text,
    mifCurrentDepositNonADA   :: [(Text, Text, Text)],
    mifCurrentDepositADA      :: Text,
    mifNextDeposit            :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type KeysResponse = [KeysResponseItem]

data KeysResponseItem = KeysResponseItem
  {
    krMixerInstanceFrontend  :: MixerInstanceFrontend,
    krDepositKeys            :: [BaseField],
    krWithdrawKeys           :: [BaseField]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data WithdrawRequest = WithdrawRequest
  {
    wrWithdrawOption         :: Either Text BaseField, -- either wallet address or the new deposit key
    wrMixerInstanceFrontend  :: MixerInstanceFrontend,
    wrSigmaProtocolInput     :: SigmaProtocolInput,
    wrSigmaProtocolProof     :: SigmaProtocolProof
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SigmaProtocolGenerateProofInput = SigmaProtocolGenerateProofInput
    {
        gpiKeyNumber      :: Integer,
        gpiWithdrawOption :: Either Text BaseField,
        gpiSecret         :: ExpField,
        gpiAddress        :: ExpField
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Find Deposit

apiSigmaProtocolFindDeposit :: ExpField -> KeysResponse -> Maybe KeysResponseItem
apiSigmaProtocolFindDeposit secret = find (apiSigmaProtocolFindDeposit' secret)

apiSigmaProtocolFindDeposit' :: ExpField -> KeysResponseItem -> Bool
apiSigmaProtocolFindDeposit' secret kri =
  let (_, g1, g2, _) = mifGenerators $ krMixerInstanceFrontend kri
      dKeys = krDepositKeys kri
      wKeys = krWithdrawKeys kri
      dKey  = pow g1 (fromZp secret)
      wKey  = pow g2 (fromZp secret)
  in dKey `notElem` dKeys || wKey `elem` wKeys

-- Generate Proof

apiSigmaProtocolGenerateProof :: SigmaProtocolGenerateProofInput -> KeysResponse -> IO (Maybe WithdrawRequest)
apiSigmaProtocolGenerateProof input resp =
  case apiSigmaProtocolFindDeposit (gpiSecret input) resp of
    Just kri -> apiSigmaProtocolGenerateProof' input kri
    Nothing  -> return Nothing

apiSigmaProtocolGenerateProof' :: SigmaProtocolGenerateProofInput -> KeysResponseItem -> IO (Maybe WithdrawRequest)
apiSigmaProtocolGenerateProof' input kri =
    let n      = gpiKeyNumber input
        wopt   = gpiWithdrawOption input
        secret = gpiSecret input
        addr   = gpiAddress input
        mif    = krMixerInstanceFrontend kri
        gens   = mifGenerators mif
        dKeys  = krDepositKeys kri
    in fmap (uncurry (WithdrawRequest wopt mif)) <$> sigmaProtocolProveRandomized gens (dKeys, secret, addr, n-1)

apiSigmaProtocolCheckFrontend :: [MixerInstanceFrontend] -> KeysResponse -> KeysResponse
apiSigmaProtocolCheckFrontend mifs = filter (\kri -> krMixerInstanceFrontend kri `elem` mifs)