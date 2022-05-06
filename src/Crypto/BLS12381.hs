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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Crypto.BLS12381 where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude
import           Prelude                           (Show, IO)
import qualified Prelude                           ((<$>))
import           System.Random                     (randomRIO)

import           Crypto.Curve                      (CurvePoint(..), EllipticCurve(..))
import           Crypto.Extension
import           Crypto.Zp                         (Zp, FiniteField(..), toZp)
import           Crypto.Polynomial                 (toPoly)

----------------------------------- BLS12381 --------------------------------

data Q = Q
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup Q where
  {-# INLINABLE (<>) #-}
  (<>) _ _ = Q

instance Monoid Q where
  {-# INLINABLE mempty #-}
  mempty = Q

instance FiniteField Q where
  {-# INLINABLE fieldPrime #-}
  fieldPrime = const 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

data R = R
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Semigroup R where
  {-# INLINABLE (<>) #-}
  (<>) _ _ = R

instance Monoid R where
  {-# INLINABLE mempty #-}
  mempty = R

-- Fr
type Fr = Zp R

instance FiniteField R where
  {-# INLINABLE fieldPrime #-}
  fieldPrime = const 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

{-# INLINABLE generateFr #-}
generateFr :: IO Fr
generateFr = toZp Prelude.<$> (randomRIO (1, fieldPrime (mempty :: R) - 1) :: IO Integer)

-- BLS12381
type T1 = Zp Q
instance EllipticCurve T1 where
    {-# INLINABLE aCurveCoef #-}
    aCurveCoef = zero
    {-# INLINABLE gen #-}
    gen = CP
      (toZp 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb)
      (toZp 0x8b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1)

-- BLS12381T
type T2 = Extension T1 E2
instance EllipticCurve T2 where
    {-# INLINABLE aCurveCoef #-}
    aCurveCoef = zero
    {-# INLINABLE gen #-}
    gen = CP
      (E (toPoly [ toZp 0x24aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8
          , toZp 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e
          ]))
      (E (toPoly [ toZp 0xce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801
          , toZp 0x606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be]))

data E2 = E2
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- deriving instance ToSchema t => ToSchema (Polynomial t)

instance Semigroup E2 where
    {-# INLINABLE (<>) #-}
    (<>) _ _ = E2

instance Monoid E2 where
    {-# INLINABLE mempty #-}
    mempty = E2

instance IrreducibleMonic T1 E2 where
    {-# INLINABLE poly #-}
    poly = const $ toPoly [one, zero, one]

data E6 = E6
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- deriving instance ToSchema t => ToSchema (Polynomial t)

instance Semigroup E6 where
    {-# INLINABLE (<>) #-}
    (<>) _ _ = E6

instance Monoid E6 where
    {-# INLINABLE mempty #-}
    mempty = E6

instance IrreducibleMonic T2 E6 where
    {-# INLINABLE poly #-}
    poly = const $ toPoly [ E (zero -
      toPoly [toZp 0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd556
        , toZp 0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd555
        ]), zero, zero, one]

data E12 = E12
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- deriving instance ToSchema t => ToSchema (Polynomial t)

instance Semigroup E12 where
    {-# INLINABLE (<>) #-}
    (<>) _ _ = E12

instance Monoid E12 where
    {-# INLINABLE mempty #-}
    mempty = E12

instance IrreducibleMonic (Extension T2 E6) E12 where
    {-# INLINABLE poly #-}
    poly = const $ toPoly [ E (toPoly [zero, zero-one]), zero, one]

--------------------------------- Pairing BLS12381 -------------------------------------

type TT = Extension (Extension T2 E6) E12

{-# INLINABLE pairing #-}
pairing :: CurvePoint T1 -> CurvePoint T2 -> TT
pairing p1 p2 = finalExponentiationBLS12 parameterHex $
  millerAlgorithmBLS12 parameterBin p1 p2

-- | [Miller algorithm for Barreto-Lynn-Scott degree 12 curves]
-- (https://eprint.iacr.org/2016/130.pdf).
{-# INLINABLE millerAlgorithmBLS12 #-}
millerAlgorithmBLS12 :: [Integer] -> CurvePoint T1 -> CurvePoint T2 -> TT
millerAlgorithmBLS12 (x:xs) p q = snd $
    millerLoop p q xs (if x > 0 then q else inv q, mempty)
millerAlgorithmBLS12 _ _ _      = mempty

-- Miller loop, line 2 to line 10.
{-# INLINABLE millerLoop #-}
millerLoop :: CurvePoint T1 -> CurvePoint T2 -> [Integer] -> (CurvePoint T2, TT) -> (CurvePoint T2, TT)
millerLoop p q = millerLoop'
  where
    millerLoop' []     tf = tf
    millerLoop' (x:xs) tf = case doublingStep p tf of
      tf2
        | x == 0    -> millerLoop' xs tf2
        | x == 1    -> millerLoop' xs $ additionStep p q tf2
        | otherwise -> millerLoop' xs $ additionStep p (inv q) tf2

-- Doubling step, line 4.
{-# INLINABLE doublingStep #-}
doublingStep :: CurvePoint T1 -> (CurvePoint T2, TT) -> (CurvePoint T2, TT)
doublingStep p (t, f) = (<>) f . (<>) f <$> lineFunction p t t

-- Addition step, line 6 and line 8.
{-# INLINABLE additionStep #-}
additionStep :: CurvePoint T1 -> CurvePoint T2 -> (CurvePoint T2, TT) -> (CurvePoint T2, TT)
additionStep p q (t, f) = (<>) f <$> lineFunction p q t

-- | [Final exponentiation for Barreto-Lynn-Scott degree 12 curves]
-- (https://eprint.iacr.org/2016/130.pdf).
{-# INLINABLE finalExponentiationBLS12 #-}
finalExponentiationBLS12 :: Integer -> TT -> TT
finalExponentiationBLS12 u = hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 f = conj f * inv f              -- f^(p^6 - 1) 
        p2 f = f * (frob . frob) f         -- f^(p^2 + 1)
    hardPart f = p4
      where
        f2 = f * f                                        -- f^2
        y3 = powUnitary (powUnitary f u * conj f2) u * f  -- f^(lambda_3)
        y2 = powUnitary y3 u                              -- f^(lambda_2)
        y1 = powUnitary y2 u * conj y3                    -- f^(lambda_1)
        y0 = powUnitary y1 u * f2 * f                     -- f^(lambda_0)
        p4 = y0 * frob (y1 * frob (y2 * frob y3))         -- f^((p^4 - p^2 + 1) / r)

-- | BLS12381 curve parameter @s = t@ in signed binary.
{-# INLINABLE parameterBin #-}
parameterBin :: [Integer]
parameterBin = [-1,-1, 0,-1, 0, 0,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, 0
                  , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
               ]

-- | BLS12381 curve parameter @t@ in hexadecimal.
{-# INLINABLE parameterHex #-}
parameterHex :: Integer
parameterHex = -0xd201000000010000

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

-- | Line function evaluation @Line(T, Q, P)@.
--
-- Compute the line function between two points @T@ and @Q@ in @G2@,
-- evaluate the line function at a point @P@ in @G1@,
-- and embed the line function evaluation in @GT@.
{-# INLINABLE lineFunction #-}
lineFunction :: CurvePoint T1 -> CurvePoint T2 -> CurvePoint T2 -> (CurvePoint T2, TT) -- ^ Points @T + Q@ and @Line(T, Q, P)@.
lineFunction (CP x y) (CP x1 y1) (CP x2 y2)
    | x1 /= x2        = (CP x3  y3 , E (toPoly [embed2 (zero - y), E (toPoly [embed x * l , y1 - l  * x1])]))
    | y1 + y2 == zero = (O         , E (toPoly [embed2          x, embed (zero - x1)]))
    | otherwise       = (CP x3' y3', E (toPoly [embed2 (zero - y), E (toPoly [embed x * l', y1 - l' * x1])]))
  where
    l   = (y2 - y1) * inv (x2 - x1)
    x3  = l * l - x1 - x2
    y3  = l * (x1 - x3) - y1
    x12 = x1 * x1
    l'  = (x12 + x12 + x12) * inv (y1 + y1)
    x3' = l' * l' - x1 - x2
    y3' = l' * (x1 - x3') - y1
lineFunction _ _ _ = (O, mempty)
