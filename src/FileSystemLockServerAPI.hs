{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystemLockServerAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import qualified Crypto.Cipher.AES as CCA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash.MD5
import           FileSystemDirectoryServerAPI hiding (API)


data FileLock = FileLock  { virtLockPath :: String
                          , virtPathLocked :: Bool
                          } deriving (Generic, ToJSON, FromJSON, ToBSON, FromBSON, Show)


-- data type for the lock request message
data LockFileReq = LockFileReq  { lockVirtPath :: String
                                } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)

-- data type for the unlock request message
data UnlockFileReq = UnlockFileReq  { unlockVirtPath :: String
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


--this API will be for the lock-service - careful - it's using the same name as the other API services
type API =  "lockFile"                    :> ReqBody '[JSON] LockFileReq :> Post '[JSON] Bool
            :<|> "unlockFile"             :> ReqBody '[JSON] UnlockFileReq :> Post '[JSON] Bool
            :<|> "discovery"          :> ReqBody '[JSON] FileSystemServerRecord :> Post '[JSON] Bool

