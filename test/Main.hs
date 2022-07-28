{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Prelude
import           Test.QuickCheck             (quickCheck)

import           Test                        


main :: IO ()
main = do
    -- t0 <- getCPUTime
    -- quickCheck prop_CorrectProof
    -- t1 <- getCPUTime
    -- print $ (fromIntegral (t1 - t0) :: Double) / 10^(12 :: Integer)

    quickCheck prop_CorrectSPProof