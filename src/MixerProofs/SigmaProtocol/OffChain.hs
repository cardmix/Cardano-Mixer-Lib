{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module MixerProofs.SigmaProtocol.OffChain where

import           PlutusTx.Prelude          hiding (mapM, (<$>), fmap)
import           Prelude                   (IO, mapM)
import           System.Random             (randomRIO)

import           Crypto
import           MixerProofs.SigmaProtocol.OnChain
import           MixerProofs.SigmaProtocol.Types
import           Utils.Common              (drop)

sigmaProtocolProveRandomized :: SigmaProtocolGenerators -> SigmaProtocolProveRandomizedInput
    -> IO (Maybe (SigmaProtocolInput, SigmaProtocolProof))
sigmaProtocolProveRandomized gens (keys, secret, addr, n) = do
    (keys', es, xs) <- sigmaProtocolRandomize gens (keys, secret, n)
    return $ sigmaProtocolProve gens (keys', es, xs, secret, addr)

-- TODO: SigmaProtocolProveInput could be incorrect!
sigmaProtocolProve :: SigmaProtocolGenerators -> SigmaProtocolProveInput -> Maybe (SigmaProtocolInput, SigmaProtocolProof)
sigmaProtocolProve gens@(_, g1, g2, g3) (keys, fakeEs, fakeXs, secret, addr) = Just (spi, spp)
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

