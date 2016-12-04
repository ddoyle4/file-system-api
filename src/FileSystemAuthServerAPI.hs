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



--DATA TYPES FOR SECURE COMMUNICATION
--The authentification server generates keys for secure communicatin between a sender (invariably the client in this case)
--and a receiver (a file server, a directory server, etc.). See notes for details about particular keys

key1Seed :: String
key1Seed = "key1seed"

key2Seed :: String
key2Seed = "key2seed"

--Cotains key 1 and metadata for a receiver
data ReceiverToken = ReceiverToken  { recKey1Seed :: String
                                    , recMetaData :: String     ---TODO spice this metadata up
                                    } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

--contains key 1 and encrypted ReceiverToken
data SenderToken = SenderToken  { senKey1Seed :: String
                                , encReceiverToken :: String
                                } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data AuthRespone = AuthResponse   { authStatus :: String    --TODO try bool instead
                                  , encSenderToken :: String
                                  } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON

data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data User = User  { username :: String
                  , userpassword :: String
                  } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

--model for how the user is stored in the database
data DBUser = DBUser  { dbusername :: String
                      , dbpassword :: String
                      , dbencusername :: String     --the user's username encrypted with their password
                      } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

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
      --NOTE instead of creating yet another data type almost identical to User, I am overloading the
      --user data type for authorisation requests - the only difference here is that the password
      --will actually be a string representation of the username encrypted with the password
      :<|> "authUser"                   :> ReqBody '[JSON] User  :> Post '[JSON] Bool          


-- ENCRYPTION STUFF -TODO perhaps this would be better in it's own API 

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
