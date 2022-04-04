{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module MixerState where

import           Data.Aeson                               (ToJSON, FromJSON)
import           GHC.Generics                             (Generic)
import           PlutusTx.Prelude                         hiding ((<>), mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger, check)
import           Prelude                                  (Show, (^))

import           Crypto
import           Utils.Common                             (drop)


----------------------- Data types, instances, and constants -----------------------------

treeSize :: Integer
treeSize = 10

data MerkleTree = MerkleTree Integer [Fr]
    deriving (Show, Generic, ToJSON, FromJSON)
type MixerState = [MerkleTree]

getMerkleLeafNumber :: MixerState -> Fr -> Maybe (Integer, Integer)
getMerkleLeafNumber state leaf = do
        k <- nTree state
        m <- nDep $ state !! k
        return (k, m + 1)
    where nDep  (MerkleTree _ tree) = findIndex (leaf ==) tree
          nTree s                   = findIndex ((/=) Nothing . nDep) s

getMerkleTree :: MixerState -> (Integer, Integer) -> Maybe MerkleTree
getMerkleTree state (k, m) = do
    MerkleTree _ tree <- if length state <= k
                            then Nothing
                            else Just $ state !! k
    if 1 > m || m > 2^treeSize
        then Nothing
        else Just $ MerkleTree m $ padToPowerOfTwo treeSize $ take m tree

constructStateFromList :: ([Fr], MixerState) -> ([Fr], MixerState)
constructStateFromList ([], state)  = ([], state)
constructStateFromList (lst, state) = constructStateFromList (drop (2 ^ treeSize) lst, state ++ [MerkleTree n tree'])
    where tree  = take (2 ^ treeSize) lst
          tree' = padToPowerOfTwo treeSize tree
          n     = length tree