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


-- Information about files on servers.
-- IMPORTANT
--  Primary File Records are inserted by the Directory Server
--  Secondary File Records are inserted by the file servers as copies of the file propagate throughout the system
data FileRecord = FileRecord  { recordType :: String        --is this a "primary" record or a "secondary" (copy) of the file
                              , fileRecordName :: String          --name of the file
                              , fileVersion :: String       --version of the file
                              , serverRecord :: FileServerRecord
                              } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read, Eq)

--represents cache entry
--IMPORTANT
--  These are inserted by the Directory Server - they are fetched from file servers according to the caching strategy
--  Note on cacheDirty and cacheFilled:
--    As described in the Directory Service README, cacheFilled false signifies that the cache is empty and requires the task scheduler to 
--    fill it on next scheduled run. cacheDirty true requries the same action from the task scheduler. However, when cacheDirty is true, any further requests
--    to write to the file will increment cacheVersion (and fileVersion for the corresponding FileRecord).
data CacheRecord = CacheRecord  { cacheName :: String     --file name
                                , cacheVersion :: String  --file version
                                , cacheData :: String
                                , cacheFilled :: Bool     -- notifies task scheduler to fill cache with file data asap
                                , cacheDirty :: Bool      -- true signifies that the file version is out-of-date and needs updating
                                , cacheAge :: String      -- represents how old this cache record is, used when clearing cache
                                } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

--data type for a request to resolve a file to a server
data ResolutionRequest = ResolutionRequest  { resolveName :: String         --name of the file
                                            , resolveIntention :: String    --whether the client intends "read" or "write" to this file
                                            , authToken :: String           --token used for cache hits to securely transfer files
                                            } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

data ResolutionResponse = ResolutionResponse  { resolutionStatus :: Bool
                                              , resolution :: FileRecord
                                              , cacheHit :: Bool            -- set to true on cache hit
                                              , cachedData :: String        -- contains file data if cache hit
                                              } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)
--TODO delete record request

--information about individual file servers
--IMPORTANT
--  These are inserted by the CLIENT as part of configuring the file system
data FileServerRecord = FileServerRecord  { fsHost :: String
                                          , fsPort :: String
                                          , fiveMinLoad :: String
                                          , currentSize :: String
                                          } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

data FileServerNotification = FileServerNotification  { fileServerRecords :: [FileServerRecord]

                                                      } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToBSON, FromBSON, Read)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String
deriving instance FromBSON Bool  -- we need these as BSON does not provide
deriving instance ToBSON   Bool
deriving instance FromBSON [FileServerRecord]  -- we need these as BSON does not provide
deriving instance ToBSON   [FileServerRecord]

-- used when responding negatively to a ResolutionRequest
nullFileServerRecord :: FileServerRecord
nullFileServerRecord = FileServerRecord "NULL" "NULL" "NULL" "NULL"

nullFileRecord :: FileRecord
nullFileRecord = FileRecord "NULL" "NULL" "NULL" nullFileServerRecord

negativeResolutionResponse :: ResolutionResponse
negativeResolutionResponse = ResolutionResponse False nullFileRecord False ""

type API =  "resolveFile"                :> ReqBody '[JSON] ResolutionRequest  :> Post '[JSON] ResolutionResponse
            :<|> "insertServerRecord"    :> ReqBody '[JSON] FileServerRecord   :> Post '[JSON] Bool 
            :<|> "insertFileRecord"      :> ReqBody '[JSON] FileRecord         :> Post '[JSON] Bool
            :<|> "transResolveFile"      :> ReqBody '[JSON] ResolutionRequest  :> Post '[JSON] ResolutionResponse

