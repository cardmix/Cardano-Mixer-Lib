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

module MixerProofs.SigmaProtocol.Types where

import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           PlutusTx.Prelude          hiding (mapM, (<$>), fmap)
import           Prelude                   (Show)

import           Crypto

--------------------------------------- Finite Fields -------------------------------------------

data FBase = FBase
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup FBase where
  {-# INLINABLE (<>) #-}
  (<>) _ _ = FBase

instance Monoid FBase where
  {-# INLINABLE mempty #-}
  mempty = FBase

type BaseField = Zp FBase

instance FiniteField FBase where
  {-# INLINABLE fieldPrime #-}
  fieldPrime = const (fieldPrime R)

data FExp = FExp
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup FExp where
  {-# INLINABLE (<>) #-}
  (<>) _ _ = FExp

instance Monoid FExp where
  {-# INLINABLE mempty #-}
  mempty = FExp

type ExpField = Zp FExp

instance FiniteField FExp where
  {-# INLINABLE fieldPrime #-}
  fieldPrime = const (fieldPrime R - 1)

------------------------------------------ Types ----------------------------------------------

-- deposit keys, withdraw key, addrExp, and addrValue
type SigmaProtocolInput = ([BaseField], BaseField, BaseField, ExpField)

-- triple of commitments
type SigmaProtocolCommit = ([BaseField], [BaseField], [BaseField])

-- commitments, errors, and responses
type SigmaProtocolProof = (SigmaProtocolCommit, [ExpField], [ExpField])

type SigmaProtocolGenerators = (BaseField, BaseField, BaseField, BaseField)

testGens :: SigmaProtocolGenerators
testGens = (Zp 100500, Zp 3, Zp 5, Zp 7)

type SigmaProtocolProveRandomizedInput = ([BaseField], ExpField, ExpField, Integer)

type SigmaProtocolRandomizeInput = ([BaseField], ExpField, Integer)

type SigmaProtocolProveInput = ([BaseField], [ExpField], [ExpField], ExpField, ExpField)
