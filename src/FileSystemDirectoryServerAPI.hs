{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystemDirectoryServerAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import qualified Crypto.Cipher.AES as CCA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash.MD5

--information about files on servers
data FileRecord = FileRecord  { recordType :: String    --is this a "primary" record or a "secondary" (copy) of the file
                              , fileName :: String      --name of the file
                              , fileVersion :: String   --version of the file
                              , serverHost :: String    --ip of server hosting file
                              , serverPort :: String    --port of server hosting file
                              } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

--represents cache entry
data CacheRecord = CacheRecord  { cacheName :: String
                                , cacheVersion :: String
                                , cacheData :: String
                                } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

--data type for a request to resolve a file to a server
data ResolutionRequest = ResolutionRequest  { resolveName :: String         --name of the file
                                            , resolveIntention :: String    --whether the client intends "read" or "write" to this file
                                            } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

--TODO delete record request

--information about individual file servers
data FileServerRecord = FileServerRecord  { fsHost :: String
                                          , fsPort :: String
                                          , fiveMinLoad :: String
                                          } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String
deriving instance FromBSON Bool  -- we need these as BSON does not provide
deriving instance ToBSON   Bool

type API =  "resolveFile"                :> ReqBody '[JSON] ResolutionRequest  :> Post '[JSON] FileRecord
            :<|> "insertServerRecord"    :> ReqBody '[JSON] FileServerRecord   :> Post '[JSON] Bool 
            :<|> "insertFileRecord"      :> ReqBody '[JSON] FileRecord         :> Post '[JSON] Bool
