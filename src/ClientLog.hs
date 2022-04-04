{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}

module ClientLog
    (
        logSecrets,
        getSecrets
    ) where

import           Data.Aeson                                   (encode, decode)
import           Data.ByteString.Lazy                         (writeFile, readFile)
import           PlutusTx.Prelude                             hiding ((<$>))
import           Prelude                                      (IO, (<$>), FilePath, show)
import           System.Directory

import           MixerUserData

fileDS :: FilePath
fileDS  = "testnet/Deposits/DepositSecret"

fileSAS :: FilePath
fileSAS = "testnet/Deposits/ShieldedAccountSecret"

logSecrets :: DepositSecret -> ShieldedAccountSecret -> IO ()
logSecrets ds sas = do
    i <- findLastFile
    writeFile (fileDS ++ show i) (encode ds)
    writeFile (fileSAS ++ show i) (encode sas)

getSecrets :: IO (Maybe (DepositSecret, ShieldedAccountSecret))
getSecrets = do
    i <- findLastFile
    if i > 0
        then do
            Just ds <- decode <$> readFile (fileDS ++ show (i-1))
            Just sas <- decode <$> readFile (fileSAS ++ show (i-1))
            removeFile (fileDS ++ show (i-1))
            removeFile (fileSAS ++ show (i-1))
            return $ Just (ds, sas)
        else return Nothing

findLastFile :: IO Integer
findLastFile = go 0
    where
        go i = do
            b <- doesFileExist $ fileDS ++ show i
            if b then go (i+1) else return i
