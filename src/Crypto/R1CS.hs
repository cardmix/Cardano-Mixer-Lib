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

module Crypto.R1CS (R1C(..), R1CS, Wires(..), Assignment, makeSub, solveR1CS, getR1CSPolynomials, loadR1CSFile) where

import           Data.Aeson                        (FromJSON, decode)
import           Data.ByteString.Lazy              (readFile)
import           Data.Map                          (Map, fromList, findWithDefault, toList, union, intersection, difference, singleton, empty)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude                  hiding ((<$>), (<*>), toList, mconcat)
import           Prelude                           (Show (..), String, IO, read, unzip3, (<*>), (^))
import qualified Prelude                           (null)

import           Crypto.BLS12381                   (Fr)
import           Crypto.DFT                        (IDFT(..), toDFT, idft, nearestPowerOfTwo, unDFT, extendDFT, unIDFT, toIDFT)
import           Crypto.Zp                         (toZp)
import           Utils.Common                      (replicate, drop)

------------------------------ R1CS -----------------------------

data R1C = R1C
    {
        leftCoefs  :: Map Integer Fr,
        rightCoefs :: Map Integer Fr,
        outCoefs   :: Map Integer Fr
    } deriving Show

type R1CS = [R1C]

data Wires = Wires Integer Integer Integer
  deriving Show

----------------------------- Assingments -----------------------

type Assignment = Map Integer Fr

{-# INLINABLE makeSub #-}
makeSub :: Assignment -> Assignment -> Fr
makeSub subs expr = sum [ v * findWithDefault zero k subs | (k, v) <- toList expr ]

{-# INLINABLE solveR1CS #-}
solveR1CS :: R1CS -> Assignment -> Assignment
solveR1CS r1cs subs = if Prelude.null (difference newSubs subs) then subs else solveR1CS r1cs newSubs
  where newSubs = foldl solveR1C subs r1cs

-- TODO: optimize the function, throw error when solving fails
{-# INLINABLE solveR1C #-}
solveR1C :: Assignment -> R1C -> Assignment
solveR1C subs r1c = subs `union` subNew
  where
    x   = [leftCoefs, rightCoefs, outCoefs] <*> [r1c] :: [Assignment]
    lst = map (makeSub subs . flip intersection subs) x
    a   = if length lst >= 3 then head lst else error ()
    b   = if length lst >= 3 then lst !! 1 else error ()
    c   = if length lst >= 3 then lst !! 2 else error ()
    diff = map (toList . flip difference subs) x
    subNew = case diff of
      [[], [], []]       -> singleton 0 one
      [[(k, v)], [], []] -> singleton k ((c * inv b - a) * inv v)
      [[], [(k, v)], []] -> singleton k ((c * inv a - b) * inv v)
      [[], [], [(k, v)]] -> singleton k ((a * b - c) * inv v)
      _                  -> singleton 0 one

-- Get polynomials (u, v, w, h)
{-# INLINABLE getR1CSPolynomials #-}
getR1CSPolynomials :: R1CS -> Assignment -> (IDFT, IDFT, IDFT, IDFT)
getR1CSPolynomials r1cs subs = (u, v, w, h)
  where
    f r1c = (makeSub subs (leftCoefs r1c), makeSub subs (rightCoefs r1c), makeSub subs (outCoefs r1c))
    (a, b, c) = unzip3 $ map f r1cs
    uDFT = toDFT a
    vDFT = toDFT b
    wDFT = toDFT c
    lDFT = uDFT * vDFT - extendDFT wDFT
    u = idft uDFT
    v = idft vDFT
    w = idft wDFT
    m = divide (length $ unDFT lDFT) 2
    h = toIDFT $ drop m (unIDFT $ idft lDFT)

----------------------- File operations ----------------------

data R1CSFile = R1CSFile
            {
                n8           :: Integer,
                prime        :: String,
                nVars        :: Integer,
                nOutputs     :: Integer,
                nPubInputs   :: Integer,
                nPrvInputs   :: Integer,
                nLabels      :: Integer,
                nConstraints :: Integer,
                constraints  :: [[Map String String]],
                map_         :: [Integer]
            }
  deriving (Show, Generic, FromJSON)

{-# INLINABLE loadR1CSFile #-}
loadR1CSFile :: String -> IO (R1CS, Wires)
loadR1CSFile str = do
    input <- readFile str
    let maybeR1CSFile = decode input :: Maybe R1CSFile
        (r1cs, wires) = decodeR1CS maybeR1CSFile
        n             = nearestPowerOfTwo $ length r1cs
    return (addEmptyR1CS n r1cs, wires)

{-# INLINABLE addEmptyR1CS #-}
addEmptyR1CS :: Integer -> R1CS -> R1CS
addEmptyR1CS _ [] = []
addEmptyR1CS n r1cs
              | l > 2^n     = r1cs
              | otherwise = r1cs ++ replicate (2^n-l) (R1C empty empty empty)
  where l = length r1cs

{-# INLINABLE decodeR1CS #-}
decodeR1CS :: Maybe R1CSFile -> (R1CS, Wires)
decodeR1CS mf = case mf of
                  Nothing -> error()
                  Just rf ->
                        let
                          r = constraints rf
                          f x = R1C {leftCoefs = g 0 x, rightCoefs = g 1 x, outCoefs = g 2 x}
                          g ind x = fromList $ map h $ toList (x !! ind)
                          h (key, val) = (read key :: Integer, toZp $ read val)
                          pubWires   = nOutputs rf + nPubInputs rf
                          privWires  = nOutputs rf + nPubInputs rf + nPrvInputs rf
                          totalWires = nConstraints rf + nPubInputs rf + nPrvInputs rf
                        in if null r then error() else (map f r, Wires pubWires privWires totalWires)
