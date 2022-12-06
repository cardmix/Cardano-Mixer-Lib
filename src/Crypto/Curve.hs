{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Crypto.Curve (CurvePoint(..), EllipticCurve(..), add, dbl, mul, fromX, addJ, dblJ, mulJ, toJ, fromJ, fromXJ) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude
import           Prelude                           (Show)
import           Test.QuickCheck.Arbitrary.Generic (Arbitrary(..), Arg, genericArbitrary)

import           Crypto.Zp                         (Zp, fromZp, FiniteField)
import           Utils.Common                      (ToIntegerData (..))
import Crypto.Extension (squareRoot)

class (Ring t, Group t, Eq t) => EllipticCurve t where
    aCurveCoef :: t
    bCurveCoef :: t
    gen        :: CurvePoint t

data CurvePoint t = CP t t | O
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance (Arg (CurvePoint t) t, Arbitrary t) => Arbitrary (CurvePoint t) where
  {-# INLINABLE arbitrary #-}
  arbitrary = genericArbitrary

instance EllipticCurve t => Semigroup (CurvePoint t) where
    {-# INLINABLE (<>) #-}
    (<>) p1 p2
        | p1 == p2  = dbl p1
        | otherwise = add p1 p2

instance EllipticCurve t => Monoid (CurvePoint t) where
    {-# INLINABLE mempty #-}
    mempty = O

instance EllipticCurve t => Group (CurvePoint t) where
    {-# INLINABLE inv #-}
    inv O        = O
    inv (CP x y) = CP x (zero-y)

instance EllipticCurve t => Eq (CurvePoint t) where
    {-# INLINABLE (==) #-}
    (==) (CP x1 y1) (CP x2 y2) = x1 == x2 && y1 == y2
    (==) (CP _ _) O            = False
    (==) O          (CP _ _)   = False
    (==) O          O          = True

{-# INLINABLE add #-}
add :: EllipticCurve t => CurvePoint t -> CurvePoint t -> CurvePoint t
add p O       = p
add O q       = q
add (CP x1 y1) (CP x2 y2)
  | x1 == x2  = O
  | otherwise = CP x3 y3
  where
    l  = (y1 - y2) * inv (x1 - x2)
    x3 = l * l - x1 - x2
    y3 = l * (x1 - x3) - y1

{-# INLINABLE dbl #-}
dbl :: EllipticCurve t => CurvePoint t -> CurvePoint t
dbl O         = O
dbl (CP x y)
  | y == zero = O
  | otherwise = CP x' y'
  where
    xx = x * x
    l  = (xx + xx + xx + aCurveCoef) * inv (y + y)
    x' = l * l - x - x
    y' = l * (x - x') - y

{-# INLINABLE mul #-}
mul :: EllipticCurve t => CurvePoint t -> Zp p -> CurvePoint t
mul p n = fromJ $ mulJ (toJ p) (fromZp n)      -- multiplication using Jacobian coordinates

{-# INLINABLE fromX #-}
fromX :: forall p. (FiniteField p, EllipticCurve (Zp p)) => Zp p -> Maybe (CurvePoint (Zp p))
fromX x = do
  y <- squareRoot $ (x * x + aCurveCoef) * x + bCurveCoef
  pure $ CP x y

--------------------- Jacobian coordinates -----------------------------

{-# INLINABLE mulJ #-}
mulJ :: EllipticCurve t => (t, t, t) -> Integer -> (t, t, t)
mulJ p n
  | n < 0           = invJ $ mulJ p (negate n)
  | n == 0          = (one, one, zero)
  | n == 1          = p
  | modulo n 2 == 0 = p'   -- 'even n' does not seem to work On-Chain
  | otherwise       = addJ p p'
  where
    p'   = mulJ (dblJ p) (divide n 2)
    invJ (x, y, z) = (x, zero-y, z)

{-# INLINABLE addJ #-}
addJ :: EllipticCurve t => (t, t, t) -> (t, t, t) -> (t, t, t)
addJ (x1, y1, z1) (x2, y2, z2)
          | z1 == zero = (x2, y2, z2)
          | z2 == zero = (x1, y1, z1)
          | x1 == x2   = (one, one, zero)
          | otherwise  = (x3, y3, z3)
  where
    two = one + one
    z1z1 = z1 * z1
    z2z2 = z2 * z2
    z1z2 = z1 + z2
    u1   = x1 * z2z2
    u2   = x2 * z1z1
    s1   = y1 * z2 * z2z2
    s2   = y2 * z1 * z1z1
    h    = u2 - u1
    h2   = two * h
    i    = h2 * h2
    j    = h * i
    r    = two * (s2 - s1)
    v    = u1 * i
    x3   = r * r - j - two * v
    y3   = r * (v - x3) - two * s1 * j
    z3   = (z1z2 * z1z2 - z1z1 - z2z2) * h

{-# INLINABLE dblJ #-}
dblJ :: EllipticCurve t => (t, t, t) -> (t, t, t)
dblJ (x1, y1, z1) = (x3, y3, z3)
  where
    two = one + one
    three = two + one
    eight = three + three + two

    a    = aCurveCoef
    xx   = x1 * x1
    yy   = y1 * y1
    yyyy = yy * yy
    zz   = z1 * z1
    xy   = x1 + yy
    yz   = y1 + z1
    s    = two * (xy * xy - xx - yyyy)
    m    = three * xx + a * zz * zz
    t    = m * m - two * s
    x3   = t
    y3   = m * (s - t) - eight * yyyy
    z3   = yz * yz - yy - zz

{-# INLINABLE fromXJ #-}
fromXJ :: forall p. (FiniteField p, EllipticCurve (Zp p)) => Zp p -> Maybe (Zp p, Zp p, Zp p)
fromXJ x = do
  y <- squareRoot $ (x * x + aCurveCoef) * x + bCurveCoef
  pure (x, y, one)

{-# INLINABLE toJ #-}
toJ :: EllipticCurve t => CurvePoint t -> (t, t, t)
toJ (CP x y) = (x, y, one)
toJ O        = (one, one, zero)

{-# INLINABLE fromJ #-}
fromJ :: EllipticCurve t => (t, t, t) -> CurvePoint t
fromJ (x, y, z)
          | z == zero = O
          | otherwise = let zz = z * z in CP (x * inv zz) (y * inv (z * zz))

instance (EllipticCurve t, ToIntegerData t) => ToIntegerData (CurvePoint t) where
  {-# INLINABLE toIntegerData #-}
  toIntegerData cp = toIntegerData x ++ toIntegerData y ++ toIntegerData z
    where (x, y, z) = toJ cp
