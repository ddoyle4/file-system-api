{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystemEncryption where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import qualified Crypto.Cipher.AES as CCA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash.MD5



-- ENCRYPTION STUFF -TODO perhaps this would be better in it's own API 

key1Seed :: String
key1Seed = "key1seed"

--seed for fileservers
key2Seed :: String
key2Seed = "key2seed"

paddingChar :: Char
paddingChar = '\0'

--adds padding char to a string for ECB encryption
addPadding :: String -> String
addPadding str = str ++ (take (16 - ((length str) `mod` 16)) (repeat paddingChar))

--removes padding char to a string for ECB decryption
removePadding :: String -> String
removePadding str = [x | x <- str, x /= paddingChar]

-- use keyString to create an AES key, and use it to encrypt toEncrypt. Return a
-- String representation of this encrypted value.
encryptString :: String -> String -> String
encryptString toEncrypt keyString = BC.unpack $ CCA.encryptECB (CCA.initAES (hash (BC.pack keyString))) (BC.pack (addPadding toEncrypt))

-- reverse of encryptString
-- NOTE: toDecrypt must have been produced by encryptString with the same
-- keyString
decryptString :: String -> String -> String
decryptString toDecrypt keyString = removePadding $ BC.unpack $ CCA.decryptECB (CCA.initAES (hash (BC.pack (keyString)))) (BC.pack toDecrypt)
