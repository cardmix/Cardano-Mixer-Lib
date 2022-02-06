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
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Crypto.Extension (Extension(..), IsExtension(..), IrreducibleMonic(..), conj, pow, powUnitary, embed, embed2) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude
import           Prelude                           (Show)
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary(..), genericArbitrary)

import           Crypto.Polynomial
import           Crypto.Zp                         (Zp, FiniteField(..))

------------------------- Extension -------------------------------------

class (Ring t, Group t, Eq t) => IsExtension t where
    deg  :: t -> Integer
    char :: t -> Integer
    frob :: t -> t

instance forall p. FiniteField p => IsExtension (Zp p) where
    {-# INLINABLE deg #-}
    deg  = const 1
    {-# INLINABLE char #-}
    char = const (fieldPrime (mempty :: p))
    {-# INLINABLE frob #-}
    frob = id

class Monoid e => IrreducibleMonic t e where
    poly :: e -> Polynomial t

newtype Extension t e = E (Polynomial t)
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary t => Arbitrary (Extension t e) where
  {-# INLINABLE arbitrary #-}
  arbitrary = genericArbitrary

instance (IsExtension t, IrreducibleMonic t e) => IsExtension (Extension t e) where
    {-# INLINABLE deg #-}
    deg _ = degPoly (poly (mempty :: e) :: Polynomial t) * deg (zero :: t)
    {-# INLINABLE char #-}
    char = const $ char (zero :: t)
    {-# INLINABLE frob #-}
    frob x@(E p) = case frobenius (unPoly p) (unPoly $ poly (mempty :: e)) of
        Just q  -> E $ toPoly q
        Nothing -> pow x $ char x
        -- pow x $ char x

frobenius :: (IsExtension t) => [t] -> [t] -> Maybe [t]
frobenius [] _ = Just []
frobenius [a] _ = Just [frob a]
frobenius [a, b] [x, _, _]
  | deg x == 2  = Just [a, negate b]
  | char x == 2 = Just [frob a - frob b * x]
  | otherwise   = Just [frob a, frob b * nxq]
  where
    nxq = pow (negate x) (divide (char x) 2)
frobenius [a, b] [x, _, _, _]
  | char x == 3 = Just [frob a - frob b * x]
  | r == 1      = Just [frob a, frob b * nxq]
  | otherwise   = Just [frob a, zero, frob b * nxq]
  where
    (q, r) = quotRem (char x) 3
    nxq    = pow (negate x) q
frobenius [a, b, c] [x, _, _, _]
  | char x == 3 = Just [frob a - (frob b - frob c * x) * x]
  | r == 1      = Just [frob a, frob b * nxq, frob c * nxq * nxq]
  | otherwise   = Just [frob a, frob c * nx * nxq * nxq, frob b * nxq]
  where
    (q, r) = quotRem (char x) 3
    nx     = negate x
    nxq    = pow nx q
frobenius _ _   = Nothing

instance IsExtension t => AdditiveSemigroup (Extension t e) where
    {-# INLINABLE (+) #-}
    (+) (E p1) (E p2) = E $ p1 + p2

instance IsExtension t => AdditiveGroup (Extension t e) where
    {-# INLINABLE (-) #-}
    (-) (E p1) (E p2) = E $ p1 - p2

instance IsExtension t => AdditiveMonoid (Extension t e) where
    {-# INLINABLE zero #-}
    zero = E zero

instance (IsExtension t, IrreducibleMonic t e) => MultiplicativeSemigroup (Extension t e) where
    {-# INLINABLE (*) #-}
    (*) (E p1) (E p2) = E $ snd $ qr (p1 * p2) (poly (mempty :: e))

instance (IsExtension t, IrreducibleMonic t e) => MultiplicativeMonoid (Extension t e) where
    {-# INLINABLE one #-}
    one = E one

instance (IsExtension t, IrreducibleMonic t e) => Semigroup (Extension t e) where
    {-# INLINABLE (<>) #-}
    (<>) = (*)

instance (IsExtension t, IrreducibleMonic t e) => Monoid (Extension t e) where
    {-# INLINABLE mempty #-}
    mempty = one

instance (IsExtension t, IrreducibleMonic t e) => Group (Extension t e) where
    {-# INLINABLE inv #-}
    inv (E a) = E (c * s)
      where
        c = fromConst $ inv $ leading g
        (g, s) = f (a, one) (poly (mempty :: e), zero)
        f (x, y) (x', y')
                    | x' == zero = (x, y)
                    | otherwise  = f (x', y') (x - q * x', y - q * y')
          where (q, _) = qr x x'

instance (IsExtension t, IrreducibleMonic t e) => Eq (Extension t e) where
    {-# INLINABLE (==) #-}
    (==) (E a) (E b) = r == zero
        where (_, r) = qr (a - b) (poly (mempty :: e))

{-# INLINABLE embed #-}
embed :: IsExtension t => t -> Extension t e
embed x = E $ fromConst x

{-# INLINABLE embed2 #-}
embed2 :: (IsExtension t, IrreducibleMonic t e1) => t -> Extension (Extension t e1) e2
embed2 = embed . embed

{-# INLINABLE conj #-}
conj :: IsExtension t => Extension t e -> Extension t e
conj (E p) = case unPoly p of
    []     -> E $ toPoly []
    [a]    -> E $ toPoly [a]
    [a, b] -> E $ toPoly [a, zero-b]
    _      -> error ()

{-# INLINABLE pow #-}
pow :: IsExtension t => t -> Integer -> t
pow a n
        | a == zero = zero
        | n == 0    = one
        | n <  0    = pow (inv a) (negate n)
        | otherwise = if r == 1 then a * pow aa q else pow aa q
    where
        (q, r) = quotRem n 2
        aa     = a * a

-- | Unitary exponentiation @^@.
--
-- Exponentiation of a unitary element @x@ to an arbitrary integer @n@
-- in a specified cyclotomic subgroup.
{-# INLINE powUnitary #-}
powUnitary :: (IsExtension t, IrreducibleMonic t e) => Extension t e -> Integer -> Extension t e
powUnitary x n
            | n < 0     = pow (conj x) (negate n)
            | otherwise = pow x n
