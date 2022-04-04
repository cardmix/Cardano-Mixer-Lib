{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Configuration.QAPConfig where

import Prelude                       (String, FilePath)

------------------------ File constants ------------------------------

fileQAPPub :: FilePath
fileQAPPub = "QAPs/qap-public"

fileQAPPriv :: FilePath
fileQAPPriv = "QAPs/qap-private"

filePublic :: FilePath
filePublic = "QAPs/CRS/public"

filePrivate :: FilePath
filePrivate = "QAPs/CRS/private"

fileCRS :: FilePath
fileCRS = "zkSNARKs/MerkleWithdraw-CRS.json"

fileReducedCRS :: FilePath
fileReducedCRS = "zkSNARKs/MerkleWithdraw-CRS-reduced.json"

fileWithdrawR1CS :: String
fileWithdrawR1CS = "QAP-compilation/cardano-mixer/MerkleWithdraw.json"







