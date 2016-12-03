{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystemAuthServerAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import qualified Crypto.Cipher.AES as CCA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash.MD5

data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data User = User  { username :: String
                  , userpassword :: String
                  } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String


data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)


type API = "load_environment_variables" :> QueryParam "name" String :> Get '[JSON] ResponseData
      :<|> "getREADME"                  :> Get '[JSON] ResponseData
      :<|> "storeMessage"               :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
      :<|> "searchMessage"              :> QueryParam "name" String :> Get '[JSON] [Message]
      :<|> "performRESTCall"            :> QueryParam "filter" String  :> Get '[JSON] ResponseData
      :<|> "debugSaveUser"              :> ReqBody '[JSON] User  :> Post '[JSON] Bool


-- ENCRYPTION STUFF

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
encryptString :: String -> String -> IO String
encryptString toEncrypt keyString = do
  let encryptBytes = BC.pack $ addPadding toEncrypt
  let k = CCA.initAES $ hash $ BC.pack keyString
  let encryptedString = BC.unpack $ CCA.encryptECB k encryptBytes
  return encryptedString

-- reverse of encryptString
-- NOTE: toDecrypt must have been produced by encryptString with the same
-- keyString
decryptString :: String -> String -> IO String
decryptString toDecrypt keyString = do
  let k = CCA.initAES $ hash $ BC.pack keyString
  let decryptedString = BC.unpack $ CCA.decryptECB k (BC.pack toDecrypt)
  let str = removePadding decryptedString
  return str

