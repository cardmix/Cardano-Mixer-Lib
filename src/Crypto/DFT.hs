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

module Crypto.DFT
  (IDFT(..), DFT(..),
   dft, idft, evalPoly,
   toIDFT, unIDFT,
   toDFT,  unDFT,
   extend, extendDFT,
   targetPolyDFT, nearestPowerOfTwo, padToPowerOfTwo) where

import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)
import           PlutusTx.Prelude
import           Prelude                           (Show, (^))

import           Crypto.BLS12381                   (Fr)
import           Crypto.Extension                  (pow)
import           Crypto.Zp                         (toZp)
import           Utils.Common                      (replicate, zipWith0, getEvenOdd)

------------------------- IDFT (polynomial coefficients) ----------------------------------------

newtype IDFT = IDFT [Fr]
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINABLE toIDFT #-}
toIDFT :: [Fr] -> IDFT
toIDFT p = IDFT $ padToPowerOfTwo l p
  where l = nearestPowerOfTwo $ length p

{-# INLINABLE unIDFT #-}
unIDFT :: IDFT -> [Fr]
unIDFT (IDFT p) = p

{-# INLINABLE extend #-}
extend :: IDFT -> IDFT
extend (IDFT a) = IDFT aExt
  where l    = nearestPowerOfTwo $ length a
        aExt = padToPowerOfTwo (l+1) a

{-# INLINABLE evalPoly #-}
evalPoly :: Fr -> IDFT -> Fr
evalPoly x (IDFT f) = sum $ zipWith (*) f y
  where y = map (pow x) [0 .. length f - 1]

instance AdditiveSemigroup IDFT where
    {-# INLINABLE (+) #-}
    (+) (IDFT x) (IDFT y) = IDFT $ zipWith0 (+) x y

instance AdditiveGroup IDFT where
    {-# INLINABLE (-) #-}
    (-) (IDFT x) (IDFT y) = IDFT $ zipWith0 (-) x y

instance AdditiveMonoid IDFT where
    {-# INLINABLE zero #-}
    zero = IDFT []

instance MultiplicativeSemigroup IDFT where
    {-# INLINABLE (*) #-}
    (*) x y = idft $ DFT $ zipWith (*) (unDFT xDFT) (unDFT yDFT)
        where xDFT = dft $ extend x
              yDFT = dft $ extend y

----------------------------------- DFTs (polynomial evaluations) ---------------------------------------

newtype DFT = DFT [Fr]
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

{-# INLINABLE toDFT #-}
toDFT :: [Fr] -> DFT
toDFT p = DFT $ padToPowerOfTwo l p
  where l = nearestPowerOfTwo $ length p

{-# INLINABLE unDFT #-}
unDFT :: DFT -> [Fr]
unDFT (DFT p) = p

{-# INLINABLE extendDFT #-}
extendDFT :: DFT -> DFT
extendDFT = dft . extend . idft

instance AdditiveSemigroup DFT where
    {-# INLINABLE (+) #-}
    (+) (DFT x) (DFT y) = DFT $ zipWith0 (+) x y

instance AdditiveGroup DFT where
    {-# INLINABLE (-) #-}
    (-) (DFT x) (DFT y) = DFT $ zipWith0 (-) x y

instance AdditiveMonoid DFT where
    {-# INLINABLE zero #-}
    zero = DFT []

instance MultiplicativeSemigroup DFT where
    {-# INLINABLE (*) #-}
    (*) x y = DFT $ zipWith0 (*) x' y'
        where DFT x' = extendDFT x
              DFT y' = extendDFT y

{-# INLINABLE dft #-}
dft :: IDFT -> DFT
dft (IDFT f) = DFT g
  where g     = dftRoot (rootPowers omega (divide n 2)) f
        omega = rootOfUnity f
        n     = length f

{-# INLINABLE idft #-}
idft :: DFT -> IDFT
idft (DFT f) = IDFT $ map (* (inv $ toZp n)) g
  where g     = dftRoot (rootPowers omega (divide n 2)) f
        omega = inv $ rootOfUnity f
        n     = length f

{-# INLINABLE dftRoot #-}
dftRoot :: [Fr] -> [Fr] -> [Fr]
dftRoot _     [x] = [x]
dftRoot omegas f   = zipWith (+) dftFst dftSnd ++ zipWith (-) dftFst dftSnd
  where (fEven, fOdd)   = getEvenOdd f
        (omegasEven, _) = getEvenOdd omegas
        dftFst  = dftRoot omegasEven fEven
        dftSnd' = dftRoot omegasEven fOdd
        dftSnd  = zipWith (*) dftSnd' omegas

--------------------------------- Target Polynomial ---------------------------------

{-# INLINABLE targetPolyDFT #-}
targetPolyDFT :: Integer -> DFT
targetPolyDFT i = DFT $ map f pointsOfT
  where
    dftLength = nearestPowerOfTwo i
    r1 = primitiveRoot dftLength
    r2 = primitiveRoot (dftLength+1)
    rootsOfT  = rootPowers r1 (2^dftLength)
    pointsOfT = rootPowers r2 (2^(dftLength+1))
    f p = foldl (\a b -> a * (p - b)) one rootsOfT

-------------------------------------------------------------------------------
-- Roots of unity
-------------------------------------------------------------------------------

{-# INLINABLE rootPowers #-}
rootPowers :: Fr -> Integer -> [Fr]
rootPowers omega n = map (pow omega) [0 .. n -1]

{-# INLINABLE padToPowerOfTwo #-}
padToPowerOfTwo :: Integer -> [Fr] -> [Fr]
padToPowerOfTwo _ [] = []
padToPowerOfTwo n x
              | l > 2^n   = x
              | otherwise = x ++ replicate (2^n-l) zero
  where l = length x

{-# INLINABLE nearestPowerOfTwo #-}
nearestPowerOfTwo :: Integer -> Integer
nearestPowerOfTwo = f 0
  where f i a = if a <= 2^i then i else f (i+1) a

{-# INLINABLE rootOfUnity #-}
rootOfUnity :: [Fr] -> Fr
rootOfUnity p = primitiveRoot $ nearestPowerOfTwo $ length p

-- | Precompute primitive roots of unity for binary powers that divide @r - 1@.
{-# INLINABLE primitiveRoot #-}
primitiveRoot :: Integer -> Fr
primitiveRoot i
    | i <= 32   = toZp $ [1,
                      52435875175126190479447740508185965837690552500527637822603658699938581184512,
                      3465144826073652318776269530687742778270252468765361963008,
                      28761180743467419819834788392525162889723178799021384024940474588120723734663,
                      35811073542294463015946892559272836998938171743018714161809767624935956676211,
                      32311457133713125762627935188100354218453688428796477340173861531654182464166,
                      6460039226971164073848821215333189185736442942708452192605981749202491651199,
                      3535074550574477753284711575859241084625659976293648650204577841347885064712,
                      21071158244812412064791010377580296085971058123779034548857891862303448703672,
                      12531186154666751577774347439625638674013361494693625348921624593362229945844,
                      21328829733576761151404230261968752855781179864716879432436835449516750606329,
                      30450688096165933124094588052280452792793350252342406284806180166247113753719,
                      7712148129911606624315688729500842900222944762233088101895611600385646063109,
                      4862464726302065505506688039068558711848980475932963135959468859464391638674,
                      36362449573598723777784795308133589731870287401357111047147227126550012376068,
                      30195699792882346185164345110260439085017223719129789169349923251189180189908,
                      46605497109352149548364111935960392432509601054990529243781317021485154656122,
                      2655041105015028463885489289298747241391034429256407017976816639065944350782,
                      42951892408294048319804799042074961265671975460177021439280319919049700054024,
                      26418991338149459552592774439099778547711964145195139895155358980955972635668,
                      23615957371642610195417524132420957372617874794160903688435201581369949179370,
                      50175287592170768174834711592572954584642344504509533259061679462536255873767,
                      1664636601308506509114953536181560970565082534259883289958489163769791010513,
                      36760611456605667464829527713580332378026420759024973496498144810075444759800,
                      13205172441828670567663721566567600707419662718089030114959677511969243860524,
                      10335750295308996628517187959952958185340736185617535179904464397821611796715,
                      51191008403851428225654722580004101559877486754971092640244441973868858562750,
                      24000695595003793337811426892222725080715952703482855734008731462871475089715,
                      18727201054581607001749469507512963489976863652151448843860599973148080906836,
                      50819341139666003587274541409207395600071402220052213520254526953892511091577,
                      3811138593988695298394477416060533432572377403639180677141944665584601642504,
                      43599901455287962219281063402626541872197057165786841304067502694013639882090,
                      937917089079007706106976984802249742464848817460758522850752807661925904159] !! i
    | otherwise = error ()