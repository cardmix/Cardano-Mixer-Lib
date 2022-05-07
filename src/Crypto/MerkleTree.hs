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

module Crypto.MerkleTree (addMerkleLeaf, getMerkleCoPath) where

import           Data.Tuple.Extra                  (snd3)
import           PlutusTx.Prelude

import           Crypto.MiMC                       (mimcHash)
import           Crypto.Zp                         (Zp, FiniteField(..))

--------------------- Merkle Tree -------------------------------------

{-# INLINABLE getMerkleCoPath #-}
getMerkleCoPath :: FiniteField p => [Zp p] -> Integer -> [Zp p]
getMerkleCoPath lst pos = snd3 $ getMerkleCoPath' (lst, [], pos)

-- TODO: do not allow zero leaf deposits in the circuit
{-# INLINABLE getMerkleCoPath' #-}
getMerkleCoPath' :: FiniteField p => ([Zp p], [Zp p], Integer) -> ([Zp p], [Zp p], Integer)
getMerkleCoPath' ([] , path, pos) = ([], path, pos)
getMerkleCoPath' ([root], path, pos) = ([], path ++ [root], pos)
getMerkleCoPath' (lst, path, pos) = getMerkleCoPath' (f lst, path ++ [lst !! coPos], q)
  where (q, r) = quotRem (pos + 1) 2
        coPos  = if r == 0 then pos else pos-2
        f []         = []
        f (x1:x2:xs) = (if x1 == zero && x2 == zero then zero else mimcHash x1 x2) : f xs
        f _          = error ()

{-# INLINABLE addMerkleLeaf #-}
addMerkleLeaf :: FiniteField p => Zp p -> Integer -> [Zp p] -> [Zp p]
addMerkleLeaf l pos oPath
    | null oPath = [l]
    | r == 0    = zero : addMerkleLeaf hashLeft  d (tail oPath)
    | otherwise = l    : addMerkleLeaf hashRight d (tail oPath)
  where
    (d, r)    = quotRem (pos + 1) 2
    hashLeft  = mimcHash l zero
    hashRight = mimcHash (head oPath) l

