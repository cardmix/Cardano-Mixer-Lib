{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores         #-}

module Test where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           Prelude                           
import           System.Random                     (randomRIO)
import           Test.QuickCheck

import           Crypto
import           MixerProofs
import           MixerState
import           MixerUserData

testPKH :: Fr
testPKH = Zp 20854110061193685199434827674222213664635638209565180150857366727406

testDepositSecret :: DepositSecret
testDepositSecret = DepositSecret {getR1 = Zp 34349201534928214048687925360304213437465308683619221158614682551434292212097,
    getR2 = Zp 11958652632845107272476787413356662260007192860256503325567485611715031431263}

testShieldedAccountSecret :: ShieldedAccountSecret
testShieldedAccountSecret = ShieldedAccountSecret {getV1 = Zp 16114080001648528005202769169848587382700750917740344188046178062013576301415,
    getV2 = Zp 21737979797033048004191169926524315745094842566971126705811875029464817819939,
    getV3 = Zp 10713162197352203917969319081312783658318875981682878193115090023178857107908}

testLeaf :: Fr
testLeaf = Zp 49540433657587630087112689401428925008449209768482241532982145422059367096457

testMixerState :: MixerState
testMixerState = [MerkleTree 1 $ padToPowerOfTwo 10 [testLeaf]]

prop_CorrectProof :: ZKProofSecret -> Bool
prop_CorrectProof secret = 
    let (lastDeposit, outs, insPub, _) = computeWithdrawWires testPKH testDepositSecret testShieldedAccountSecret testMixerState
        subs = [Zp 1] ++ outs ++ insPub
        proof = simulate withdrawSecret secret withdrawCRS subs
    in verifyWithdraw subs proof