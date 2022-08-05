{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module MixerProofs.SigmaProtocol.OnChain where

import           PlutusTx.Prelude          hiding (mapM, (<$>), fmap)

import           Crypto
import           MixerProofs.SigmaProtocol.Types


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
