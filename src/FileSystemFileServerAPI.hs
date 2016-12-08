{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystemFileServerAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import qualified Crypto.Cipher.AES as CCA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash.MD5

--TODO use Ints instead of Strings for version numbers, ToBSON and FromBSON have issues
--TODO add a flag to say whether or not this file has been replicated or not - have the task scheduler check this flag regularly and replicate
--with this for some reason
-- model for file in DB
data DBFile = DBFile  { fileName :: String      -- the name of the file
                      , fileVersion :: String
                      , fileData :: String
                      } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)




-- Requests to write to a file
data WriteFileReq = WriteFileReq  { writeReqToken :: String
                                  , writeReqFileName :: String           --might put this in reqValue instead
                                  , writeReqValue :: String              --file encrypted with 'key 1'
                                  } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

data WriteFileResp = WriteFileResp  { writeStatus :: Bool
                                    , newFileVersion :: String
                                    } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

-- Requests to read from a file
data ReadFileReq = ReadFileReq  { readReqFileToken :: String
                                , readReqFileName :: String
                                } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)


data ReadFileResp = ReadFileResp  { readStatus :: Bool
                                  , readMessage :: String
                                  , encryptedResult :: String
                                  , currentFileVersion :: String
                                  } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

-- Secure Token to encrypt / decrypt data
data SecureToken = SecureToken  


deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String
deriving instance FromBSON Bool  -- we need these as BSON does not provide
deriving instance ToBSON   Bool

type API =  "writeToFile"         :> ReqBody '[JSON] WriteFileReq :> Post '[JSON] WriteFileResp
            :<|> "readFromFile"     :> ReqBody '[JSON] ReadFileReq  :> Post '[JSON] ReadFileResp

