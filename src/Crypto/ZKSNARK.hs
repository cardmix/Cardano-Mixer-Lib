{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NumericUnderscores         #-}

module Crypto.ZKSNARK where

import           Data.Aeson                        (FromJSON(..), ToJSON(..), encode, decode)
import           Data.ByteString.Lazy              (writeFile, readFile)
import           Data.Map                          (Map, toList, findWithDefault)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude                  hiding ((<$>), (<*>), toList, mapM)
import           Prelude                           (Show (..), IO, (<$>), (<*>), FilePath, init, String, last, putStrLn, print)
import qualified Prelude                           (mconcat, mapM)
import           Test.QuickCheck                   (Arbitrary(..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary)

import           Configuration.QAPConfig
import           Crypto.BLS12381                   (Fr, T1, T2, pairing, generateFr)
import           Crypto.Curve                      (CurvePoint (..), EllipticCurve(..), mul)
import           Crypto.DFT
import           Crypto.Extension                  (pow)
import           Crypto.R1CS                       (R1CS, R1C(..), Wires(..), Assignment, getR1CSPolynomials)
import           Utils.Common                      (replicate, numBatches, selectBatch)

----------------------- ZKSNARK data types ---------------------------

-- Setup

data ZKSetupSecret = ZKSetupSecret
  {
    secretAlpha :: Fr,
    secretBeta  :: Fr,
    secretGamma :: Fr,
    secretDelta :: Fr,
    secretX     :: Fr
  } deriving (Show, Generic)
instance Arbitrary ZKSetupSecret where
  arbitrary = genericArbitrary

-- Arguments: (public inputs + public outputs) (total inputs + outputs) (total inputs + intermediate + outputs) (r1cs)
data SetupArguments = SetupArguments
  {
    setupR1CS    :: R1CS,
    setupWires   :: Wires
  }

data ReferenceString = ReferenceString
  {
    refGa      ::  CurvePoint T1,
    refGb      ::  CurvePoint T1,
    refHb      ::  CurvePoint T2,
    refHg      ::  CurvePoint T2,
    refGd      ::  CurvePoint T1,
    refHd      ::  CurvePoint T2,
    refGxi     :: [CurvePoint T1],
    refHxi     :: [CurvePoint T2],
    refGpub    :: [CurvePoint T1],
    refGpriv   :: [CurvePoint T1],
    refGtarget :: [CurvePoint T1]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReducedReferenceString = ReducedReferenceString
  {
    refRedGa      ::  CurvePoint T1,
    refRedHb      ::  CurvePoint T2,
    refRedHg      ::  CurvePoint T2,
    refRedHd      ::  CurvePoint T2,
    refRedGpub    :: [CurvePoint T1]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINABLE emptyCRS #-}
emptyCRS :: ReferenceString
emptyCRS = ReferenceString gen gen gen gen gen gen [] [] [] [] []

-- Prove

data ZKProofSecret = ZKProofSecret
  {
    secretR :: Fr,
    secretS :: Fr
  } deriving (Show, Generic)
instance Arbitrary ZKProofSecret where
  arbitrary = genericArbitrary

data ProveArguments = ProveArguments SetupArguments ReferenceString Assignment

data Proof = Proof (CurvePoint T1) (CurvePoint T2) (CurvePoint T1)
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Verify

data VerifyArguments = VerifyArguments ReducedReferenceString [Fr] Proof
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

---------------------- Generate Operations ---------------------------

generateSetupSecret :: IO ZKSetupSecret
generateSetupSecret = ZKSetupSecret <$> generateFr <*> generateFr <*> generateFr <*> generateFr <*> generateFr

generateCRS :: SetupArguments -> IO ()
generateCRS sa = do
      secret <- generateSetupSecret
      qapData <- computeQAPData secret sa
      -- qapData <- loadQAPData      
      let crs    = setup secret (setupR1CS sa) qapData
          redCRS = reduceCRS crs
      writeFile fileCRS (encode crs)
      writeFile fileReducedCRS (encode redCRS)
      print secret
      putStrLn $ crsToHaskell redCRS

generateProofSecret :: IO ZKProofSecret
generateProofSecret = ZKProofSecret <$> generateFr <*> generateFr

generateProof :: ProveArguments -> IO Proof
generateProof pa = do
      secret <- generateProofSecret
      return $ prove secret pa

---------------------- Groth's zk-SNARKs--------------------

setup :: ZKSetupSecret -> R1CS -> QAPData -> ReferenceString
setup (ZKSetupSecret a b g d x) r1cs (QAPData pubExp privExp target) = ReferenceString
    {
      refGa = mul gen a,
      refGb = mul gen b,
      refHb = mul gen b,
      refHg = mul gen g,
      refGd = mul gen d,
      refHd = mul gen d,
      refGxi = mul gen <$> xi,
      refHxi = mul gen <$> xi,
      refGpub = mul gen <$> pubExp,
      refGpriv = mul gen <$> privExp,
      refGtarget = mul gen <$> targetExp
    }
  where
    n = length r1cs
    xi = map (pow x) [0..n-1]
    tx = evalPoly x (idft target) * inv d
    targetExp = zipWith (*) xi (replicate n tx)

{-# INLINABLE reduceCRS #-}
reduceCRS :: ReferenceString -> ReducedReferenceString
reduceCRS crs = ReducedReferenceString (refGa crs) (refHb crs) (refHg crs) (refHd crs) (refGpub crs)

prove :: ZKProofSecret -> ProveArguments -> Proof
prove (ZKProofSecret r s) (ProveArguments (SetupArguments r1cs (Wires l _ _)) crs subs) = proof
  where
    (IDFT u, IDFT v, _, IDFT h) = getR1CSPolynomials r1cs subs
    ai = map snd (dropWhile (\(k, _) -> k <= l) (toList subs))
    a' = refGa crs <> mconcat (zipWith mul (refGxi crs) u)
    a  = a' <> mul (refGd crs) r
    b' = refGb crs <> mconcat (zipWith mul (refGxi crs) v)
    b  = refHb crs <> mconcat (zipWith mul (refHxi crs) v) <> mul (refHd crs) s
    c  = mconcat (zipWith mul (refGpriv crs) ai) <> mconcat (zipWith mul (refGtarget crs) h) <>
         mul a' s <> mul b' r <> mul (refGd crs) (r * s)
    proof = Proof a b c

simulate :: ZKSetupSecret -> ZKProofSecret -> ReducedReferenceString -> [Fr] -> Proof
simulate (ZKSetupSecret sa sb sg sd _) (ZKProofSecret r s) crs subs = proof
  where
    g = mconcat (zipWith mul (refRedGpub crs) subs)
    a = mul gen r
    b = mul gen s
    c = mul gen ((r * s - sa * sb) * inv sd) <> inv (mul g (sg * inv sd))
    proof = Proof a b c

{-# INLINABLE verify #-}
verify :: ReducedReferenceString -> [Fr] -> Proof -> Bool
verify crs subs (Proof a b c) = lhs == rhs
  where
    g = mconcat (zipWith mul (refRedGpub crs) subs)
    lhs = pairing a b
    rhs = pairing (refRedGa crs) (refRedHb crs) * pairing g (refRedHg crs) * pairing c (refRedHd crs)


------------------------------  QAPs ----------------------------

data QAPPoly = QAPPoly
    {
      coefU :: IDFT,
      coefV :: IDFT,
      coefW :: IDFT
    }
    deriving (Show, Generic, ToJSON, FromJSON)

type QAP = [QAPPoly]

data QAPData = QAPData [Fr] [Fr] DFT
  deriving (Show, Generic, ToJSON, FromJSON)

-- Function that writes polynomials forming QAP to files
compileQAP :: SetupArguments -> IO ()
compileQAP (SetupArguments r1cs (Wires l _ m')) = do
    let batchSize  = 100
    Prelude.mconcat $ map (\j -> writeFile (fileQAPPub ++ show j) (encode $ qap $ selectBatch batchSize 0 l j)) [0 .. numBatches batchSize 0 l - 1]
    Prelude.mconcat $ map (\j -> writeFile (fileQAPPriv ++ show j) (encode $ qap $ selectBatch batchSize (l+1) m' j)) [0 .. numBatches batchSize (l+1) m' - 1]
  where
    qap :: (Integer, Integer) -> QAP
    qap (j1, j2)
      | j1 > j2   = []
      | otherwise = QAPPoly (f leftCoefs j1) (f rightCoefs j1) (f outCoefs j1) : qap (j1+1, j2)
    f :: (R1C -> Map Integer Fr) -> Integer -> IDFT
    f g i = idft $ toDFT $ map (findWithDefault zero i . g) r1cs

-- Function that computes the public and private exponents as well as loads the target polynomial
computeQAPData :: ZKSetupSecret -> SetupArguments -> IO QAPData
computeQAPData (ZKSetupSecret a b g d x) (SetupArguments r1cs (Wires l _ m')) = do
    let batchSize  = 100
        n = length r1cs
    mapM_ (f g fileQAPPub filePublic)
     ([0 .. numBatches batchSize 0 l - 1] :: [Integer])
    mapM_ (f d fileQAPPriv filePrivate)
     ([0 .. numBatches batchSize (l+1) m' - 1] :: [Integer])
    pubExp <- concat <$> Prelude.mapM (\i -> fromMaybe [] . decode <$> readFile (filePublic ++ show i)) ([0 .. numBatches batchSize 0 l - 1] :: [Integer])
    privExp <- concat <$> Prelude.mapM (\i -> fromMaybe [] . decode <$> readFile (filePrivate ++ show i)) ([0 .. numBatches batchSize (l+1) m' - 1] :: [Integer])
    return $ QAPData pubExp privExp (targetPolyDFT n)
  where
        f :: Fr -> FilePath -> FilePath -> Integer -> IO ()
        f c fRead fWrite i = do
          q <- fromMaybe [] . decode <$> readFile (fRead ++ show i)
          let p = map (evalSetupPoly q (a, b, c, x)) [0 .. length q - 1]
          writeFile (fWrite ++ show i) (encode p)

-------------------------------- Auxiliary functions -----------------------------

-- Get polynomial evaluation for CRS
evalSetupPoly :: QAP -> (Fr, Fr, Fr, Fr) -> Integer -> Fr
evalSetupPoly q (a, b, c, x) i = evalPoly x $ toIDFT p
  where
    uiX = map (b * inv c *) $ unIDFT $ coefU (q !! i)
    viX = map (a * inv c *) $ unIDFT $ coefV (q !! i)
    wiX = map (inv c *) $ unIDFT $ coefW (q !! i)
    p   = zipWith (+) (zipWith (+) uiX viX) wiX

-- Produce the Haskell code for CRS
crsToHaskell :: ReducedReferenceString -> String
crsToHaskell crs = concat (["refRedGa = \n", show (refRedGa crs) ++ ",\n",
                            "refRedHb = \n", show (refRedHb crs) ++ ",\n",
                            "refRedHg = \n", show (refRedHg crs) ++ ",\n",
                            "refRedHd = \n", show (refRedHd crs) ++ ",\n",
                            "refRedGpub = [\n", concatMap (\a -> show a ++ ",\n") (init $ refRedGpub crs),
                            show (last $ refRedGpub crs), "\n]"] :: [String])