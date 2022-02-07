{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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

module Crypto.Polynomial (Polynomial(..), degPoly, leading, unPoly, toPoly, fromConst, qr, remainderPoly, monomialMultiply, removeZeroTerms) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude
import           Prelude                           (Show)
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary(..), genericArbitrary)

import           Utils.Common


------------------------- Polynomials --------------------------------

newtype Polynomial t = P [t]
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary t => Arbitrary (Polynomial t) where
  {-# INLINABLE arbitrary #-}
  arbitrary = genericArbitrary

instance (Ring t, Eq t) => AdditiveSemigroup (Polynomial t) where
    {-# INLINABLE (+) #-}
    (+) (P x) (P y) = removeZeroTerms $ P $ zipWith0 (+) x y

instance (Ring t, Eq t) => AdditiveGroup (Polynomial t) where
    {-# INLINABLE (-) #-}
    (-) (P x) (P y) = removeZeroTerms $ P $ zipWith0 (-) x y

instance (Ring t, Eq t) => AdditiveMonoid (Polynomial t) where
    {-# INLINABLE zero #-}
    zero = P []

instance (Ring t, Eq t) => MultiplicativeSemigroup (Polynomial t) where
    {-# INLINABLE (*) #-}
    (*) (P [])       _          = zero
    (*) _           (P [])      = zero
    (*) (P [x1])    (P [y1])    = P [x1*y1]
    (*) (P [x1,x2]) (P [y1])    = P [x1*y1, x2*y1]
    (*) (P [x1])    (P [y1,y2]) = P [x1*y1, x1*y2]
    (*) (P [x1,x2]) (P [y1,y2]) = P [x1*y1, x1*y2+x2*y1, x2*y2]
    (*) (P x) (P y) = P $ coef 0
        where
            coef :: Integer -> [t]
            coef k
                    | lst == [] = []
                    | otherwise = sum lst : coef (k+1)
                where
                    lst = [ xi * yi  | (xi, ni) <- x', (yi, mi) <- y', ni + mi == k]
            x' = addNumber x 0
            y' = addNumber y 0
            addNumber :: [t] -> Integer -> [(t, Integer)]
            addNumber []     _ = []
            addNumber (a:as) n = (a, n) : addNumber as (n+1)

instance (Ring t, Eq t) => MultiplicativeMonoid (Polynomial t) where
    {-# INLINABLE one #-}
    one = P [one]

instance (Ring t, Eq t) => Semigroup (Polynomial t) where
    {-# INLINABLE (<>) #-}
    (<>) = (*)

instance (Ring t, Eq t) => Monoid (Polynomial t) where
    {-# INLINABLE mempty #-}
    mempty = one

instance Eq t => Eq (Polynomial t) where
    {-# INLINABLE (==) #-}
    (==) (P x) (P y) = x == y

{-# INLINABLE degPoly #-}
degPoly :: Polynomial t -> Integer
degPoly (P p)  = length p - 1

{-# INLINABLE leading #-}
leading :: Ring t => Polynomial t -> t
leading (P p)
    | null p    = zero
    | otherwise = last p

{-# INLINABLE unPoly #-}
unPoly :: Polynomial t -> [t]
unPoly (P p) = p

{-# INLINABLE toPoly #-}
toPoly :: (Ring t, Eq t) => [t] -> Polynomial t
toPoly = removeZeroTerms . P

{-# INLINABLE fromConst #-}
fromConst :: (Ring t, Eq t) => t -> Polynomial t
fromConst a
        | a == zero = P []
        | otherwise = P [a]

{-# INLINABLE qr #-}
qr :: (Ring t, Group t, Eq t) => Polynomial t -> Polynomial t -> (Polynomial t, Polynomial t)
qr x y = quotientRemainder x y zero

{-# INLINABLE remainderPoly #-}
remainderPoly :: (Ring t, Group t, Eq t) => Polynomial t -> Polynomial t -> Polynomial t
remainderPoly (P [x1, x2, x3]) (P [y1, y2, y3]) = removeZeroTerms $ P [x1 - a*y1, x2 - a*y2]
    where a = x3 * inv y3
remainderPoly x y
                    | n < 0       = x
                    | otherwise   = remainderPoly x' y
    where
        a    = leading x * inv (leading y)
        n    = degX - degY
        y'   = monomialMultiply y (a, n)
        x'   = x - y'
        degX = degPoly x
        degY = degPoly y

{-# INLINABLE quotientRemainder #-}
quotientRemainder :: (Ring t, Group t, Eq t) => Polynomial t -> Polynomial t -> Polynomial t -> (Polynomial t, Polynomial t)
quotientRemainder x y q
                    | n < 0       = (q, x)
                    | otherwise   = quotientRemainder x' y q'
    where
        a    = leading x * inv (leading y)
        n    = degX - degY
        y'   = monomialMultiply y (a, n)
        x'   = x - y'
        q'   = q + monomialMultiply one (a, n)
        degX = degPoly x
        degY = degPoly y

{-# INLINABLE monomialMultiply #-}
monomialMultiply :: (Ring t, Eq t) => Polynomial t -> (t, Integer) -> Polynomial t
monomialMultiply (P p) (a, n)
                    | a == zero = zero
                    | otherwise = P $ replicate n zero ++ map (a *) p

{-# INLINABLE removeZeroTerms #-}
removeZeroTerms :: (Ring t, Eq t) => Polynomial t -> Polynomial t
removeZeroTerms (P p)
                    | p == []        = P []
                    | last p == zero = removeZeroTerms (P $ init p)
                    | otherwise      = P p
