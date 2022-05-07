{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Test where

import           Data.Aeson                        (decode)
import           Data.ByteString.Lazy              (readFile)
import           Data.Map                          (fromList)
import           Data.Maybe                        (fromMaybe)
import           Prelude                           hiding (readFile)
import           System.CPUTime                    (getCPUTime)

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
    let (_, outs, insPub, _) = computeWithdrawWires testPKH testDepositSecret testShieldedAccountSecret testMixerState
        subs = [Zp 1] ++ outs ++ insPub
        proof = simulate withdrawSecret secret withdrawCRS subs
    in verifyWithdraw subs proof

test_Prove :: IO ()
test_Prove = do
    t0 <- getCPUTime
    (r1cs, wires) <- loadR1CSFile "r1cs.json"
    crs <- fromMaybe emptyCRS . decode <$> readFile "crs.json"
    let sa = SetupArguments r1cs wires
        (_, _, insPub, insPriv) = computeWithdrawWires testPKH testDepositSecret testShieldedAccountSecret testMixerState
    -- constructing witness
    let w  = fromList $ zip ((0 :: Integer) : [6..37])
            ([Zp 1] ++ insPub ++ insPriv)
        sol = solveR1CS r1cs w
        pa = ProveArguments sa crs sol
    t1 <- getCPUTime
    print $ (fromIntegral (t1 - t0) :: Double) / 10^(12 :: Integer)
    secret <- generateProofSecret
    let proof = prove secret pa
    print proof