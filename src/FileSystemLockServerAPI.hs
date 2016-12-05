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



-- data type for the lock request message
data LockFileReq = LockFileReq  { lockVirtPath :: String
                                } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)

-- data type for the unlock request message
data UnlockFileReq = UnlockFileReq  { unlockVirtPath :: String
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

--this API will be for the lock-service - careful - it's using the same name as the other API services
type API =  "lockFile"                    :> ReqBody '[JSON] LockFileReq :> Post '[JSON] Bool
            :<|> "unlockFile"             :> ReqBody '[JSON] UnlockFileReq :> Post '[JSON] Bool

