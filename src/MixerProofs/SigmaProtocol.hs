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

module MixerProofs.SigmaProtocol where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           PlutusTx.Prelude          hiding (mapM, (<$>))
import           Prelude                   (IO, Show, mapM)
import           System.Random             (randomRIO)

import           Crypto
import           Utils.Common              (drop)

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

-- leafs, key, addrExp, and addrValue
type SigmaProtocolInput = ([BaseField], BaseField, BaseField, ExpField)

-- triple of commitments
type SigmaProtocolCommit = ([BaseField], [BaseField], [BaseField])

-- commitments, errors, and responses
type SigmaProtocolProof = (SigmaProtocolCommit, [ExpField], [ExpField])

type SigmaProtocolGenerators = (BaseField, BaseField, BaseField, BaseField)

testGens :: SigmaProtocolGenerators
testGens = (Zp 100500, Zp 3, Zp 5, Zp 7)

type SigmaProtocolGenerateProofInput = ([BaseField], ExpField, ExpField, Integer)

type SigmaProtocolRandomizeInput = ([BaseField], ExpField, Integer)

type SigmaProtocolProveInput = ([BaseField], [ExpField], [ExpField], ExpField, ExpField)

-- mixer instance hashes, deposit keys, used keys
type KeysResponse = [(Integer, [BaseField], [BaseField])]

data WithdrawRequest = WithdrawRequest
  {
    wrMixerInstanceHash  :: Integer,              -- mixer instance hash
    wrSigmaProtocolInput :: SigmaProtocolInput,
    wrSigmaProtocolProof :: SigmaProtocolProof,
    wrWithdrawOption     :: Either Text BaseField -- either wallet address or the new deposit key
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON) 

----------------------------------------- On-chain ---------------------------------------------

{-# INLINABLE sigmaProtocolChallenge #-}
sigmaProtocolChallenge :: SigmaProtocolGenerators -> SigmaProtocolCommit -> ExpField
sigmaProtocolChallenge (g0, _, _, _) (as, bs, cs) = toZp $ fromZp $ pow g0 $ fromZp $ sum (as ++ bs ++ cs)

{-# INLINABLE sigmaProtocolVerify #-}
sigmaProtocolVerify :: SigmaProtocolGenerators -> SigmaProtocolInput -> SigmaProtocolProof -> Bool
sigmaProtocolVerify gens@(_, g1, g2, g3) (leafs, key, addrExp, addr) (commit@(as, bs, cs), es, xs) = eq1 && eq2 && eq3 && eq4 && eq5
    where
        ys  = xs
        zs  = map (addr *) xs
        s   = sigmaProtocolChallenge gens commit

        eq1 = all (\(a, (e, (x, l))) -> pow g1 (fromZp x) == a * pow l (fromZp e)) $ zip as $ zip es $ zip xs leafs
        eq2 = all (\(b, (e, y)) -> pow g2 (fromZp y) == b * pow key (fromZp e)) $ zip bs $ zip es ys
        eq3 = all (\(c, (e, z)) -> pow g3 (fromZp z) == c * pow addrExp (fromZp e)) $ zip cs $ zip es zs
        eq4 = s == sum es

        f :: [[a]] -> Bool
        f   = all (\lst -> length lst == length leafs)
        eq5 = f [as, bs, cs] && f [es, xs]

----------------------------------------- Off-chain --------------------------------------------

sigmaProtocolGenerateProof :: SigmaProtocolGenerators -> SigmaProtocolGenerateProofInput -> IO (SigmaProtocolInput, SigmaProtocolProof)
sigmaProtocolGenerateProof gens (keys, secret, addr, n) = do
    (keys', es, xs) <- sigmaProtocolRandomize gens (keys, secret, n-1)
    return $ sigmaProtocolProve gens (keys', es, xs, secret, addr)

sigmaProtocolProve :: SigmaProtocolGenerators -> SigmaProtocolProveInput -> (SigmaProtocolInput, SigmaProtocolProof)
sigmaProtocolProve gens@(_, g1, g2, g3) (keys, fakeEs, fakeXs, secret, addr) = (spi, spp)
    where
        key     = pow g2 (fromZp secret)
        addrExp = pow g3 (fromZp (addr * secret))
        spi     = (keys, key, addrExp, addr)

        as   = map (\(e, (x, l)) -> if pow g1 (fromZp secret) == l then pow g1 (fromZp x) else pow g1 (fromZp x) * pow l (negate $ fromZp e)) $
            zip fakeEs $ zip fakeXs keys
        bs   = map (\(e, (x, l)) -> if pow g1 (fromZp secret) == l then pow g2 (fromZp x) else pow g2 (fromZp x) * pow key (negate $ fromZp e)) $
            zip fakeEs $ zip fakeXs keys
        cs   = map (\(e, (x, l)) -> if pow g1 (fromZp secret) == l then pow g3 (fromZp (addr * x)) else pow g3 (fromZp (addr * x)) * pow addrExp (negate $ fromZp e)) $
            zip fakeEs $ zip fakeXs keys
        coms = (as, bs, cs)
        s    = sigmaProtocolChallenge gens coms
        e'   = s - sum (map (\(e, l) -> if pow g1 (fromZp secret) == l then Zp 0 else e) $ zip fakeEs keys)
        es   = map (\(e, l) -> if pow g1 (fromZp secret) == l then e' else e) $ zip fakeEs keys
        xs   = map (\(x, l) -> if pow g1 (fromZp secret) == l then x + e' * secret else x) $ zip fakeXs keys
        spp  = (coms, es, xs)

sigmaProtocolRandomize :: SigmaProtocolGenerators -> SigmaProtocolRandomizeInput -> IO ([BaseField], [ExpField], [ExpField])
sigmaProtocolRandomize (_, g1, _, _) (keys, secret, n) = do
    let l' = pow g1 (fromZp secret)
    keys' <- pickOne (filter (/= l') keys) (n-1)
    let keys'' = sort $ l' : keys'
    es <- mapM (const generateZp) keys''
    xs <- mapM (const generateZp) keys''
    return (keys'', es, xs)
    where
        pickOne :: [a] -> Integer -> IO [a]
        pickOne []  _ = return []
        pickOne _   0 = return []
        pickOne lst k = do
            let l = length lst - k + 1
            i    <- randomRIO (0, l-1)
            lst' <- pickOne (drop (i+1) lst) (k-1)
            return (lst !! i : lst')

