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
--IMPORTANT
--  Primary File Records are inserted by the Directory Server
--  Secondary File Records are inserted by the file servers as copies of the file propagate throughout the system
data FileRecord = FileRecord  { recordType :: String        --is this a "primary" record or a "secondary" (copy) of the file
                              , fileName :: String          --name of the file
                              , fileVersion :: String       --version of the file
                              , server :: FileServerRecord
                              } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read, Eq)

--represents cache entry
--IMPORTANT
--  These are inserted by the Directory Server - they are fetched from file servers according to the caching strategy
data CacheRecord = CacheRecord  { cacheName :: String
                                , cacheVersion :: String
                                , cacheData :: String
                                , cacheFilled :: Bool   -- notifies task scheduler to fill cache with file data asap
                                } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

--data type for a request to resolve a file to a server
data ResolutionRequest = ResolutionRequest  { resolveName :: String         --name of the file
                                            , resolveIntention :: String    --whether the client intends "read" or "write" to this file
                                            } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

data ResolutionResponse = ResolutionResponse  { resolutionStatus :: Bool
                                              , resolution :: FileRecord
                                              } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)
--TODO delete record request

--information about individual file servers
--IMPORTANT
--  These are inserted by the CLIENT as part of configuring the file system
data FileServerRecord = FileServerRecord  { fsHost :: String
                                          , fsPort :: String
                                          , fiveMinLoad :: String
                                          } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String
deriving instance FromBSON Bool  -- we need these as BSON does not provide
deriving instance ToBSON   Bool

-- used when responding negatively to a ResolutionRequest
nullResolutionResponse :: ResolutionResponse
nullResolutionResponse = ResolutionResponse "NULL" "NULL" "NULL" "NULL" "NULL"


type API =  "resolveFile"                :> ReqBody '[JSON] ResolutionRequest  :> Post '[JSON] ResolutionResponse
            :<|> "insertServerRecord"    :> ReqBody '[JSON] FileServerRecord   :> Post '[JSON] Bool 
            :<|> "insertFileRecord"      :> ReqBody '[JSON] FileRecord         :> Post '[JSON] Bool
