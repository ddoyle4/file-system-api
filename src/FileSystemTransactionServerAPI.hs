{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystemTransactionServerAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import qualified Crypto.Cipher.AES as CCA
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Crypto.Hash.MD5
import           FileSystemDirectoryServerAPI hiding (API)

-- ENDPOINT DATA TYPES
-- This is implemented to make it easy to add parameters to the request later
data InitTransReq = InitTransReq    deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


data InitTransResp = InitTransResp  { initTransID :: String       -- Newly create ID for this transaction
                                    , initReqStatus :: Bool       -- Wether this initialisation was successful
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


data CommitReq = CommitReq          { commitReqTransID :: String  -- Request to commit this ID
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


data CommitResp = CommitResp        { commitRespTransID :: String -- Status for this ID
                                    , commitStatus :: Bool        -- Status of commit
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


data AbortReq = AbortReq            { abortReqTransID :: String
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


data AbortResp = AbortResp          { abortResTransID :: String
                                    , abortStatus :: Bool
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


data ActionReq = ActionReq          { actionReqTransID :: String  -- Add to this transaction
                                    , actionReqType :: String        -- READ/WRITE 
                                    , encActionFileName :: String
                                    , encActionFileValue :: String    -- if write, this is the encrypted file
                                    , actionReqToken :: String
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)


data ActionResp = ActionResp        { actionRespTransID :: String
                                    , actionRespStatus :: Bool
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)
-- DATABASE DATA TYPES
-- TODO - do this for all of the other dbs in this system
transactionDBName :: String
transactionDBName = "transactions"

actionDBName :: String
actionDBName = "actions"

serverRecordsDBName :: String 
serverRecordsDBName = "server-records"

data TransactionStatus = Committed | Aborted | Building

instance Show TransactionStatus where
  show Committed  = "COMMITTED"
  show Aborted    = "ABORTED"
  show Building   = "BUILDING"

data Transaction = Transaction      { transactionID :: String
                                    , transactionStatus :: String -- "COMMITTED", "ABORTED", "BUILDING"
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)

data Action = Action                { actionID :: String
                                    , actionTransID :: String
                                    , actionServer :: FileServerRecord
                                    , actionType :: String
                                    , actionFileName :: String
                                    , actionFileValue :: String
                                    , actionStatus :: String
                                    } deriving (Show, Generic, ToJSON, FromJSON, FromBSON, ToBSON, Read)

--deriving instance FromBSON String  -- we need these as BSON does not provide
--deriving instance ToBSON   String
--deriving instance FromBSON Bool    -- we need these as BSON does not provide
--deriving instance ToBSON   Bool

type API =  "initTransaction"         :> ReqBody '[JSON] InitTransReq   :> Post '[JSON] InitTransResp
            :<|> "commitTransaction"  :> ReqBody '[JSON] CommitReq      :> Post '[JSON] CommitResp
            :<|> "abortTransaction"   :> ReqBody '[JSON] AbortReq       :> Post '[JSON] AbortResp
            :<|> "action"             :> ReqBody '[JSON] ActionReq      :> Post '[JSON] ActionResp

