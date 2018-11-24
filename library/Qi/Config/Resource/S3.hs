{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.Resource.S3 where

import           Control.Lens
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.ByteString     (ByteString)
import           Data.Default        (Default, def)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import           GHC.Show            (Show (..))
import           Protolude
import           Qi.Config.Id
import           Qi.Config.Types


newtype S3Key = S3Key Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data S3Object = S3Object {
    _s3oBucketId :: Id S3BucketId
  , _s3oKey      :: S3Key
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

s3Object
  :: Id S3BucketId
  -> S3Key
  -> S3Object
s3Object = S3Object

data S3Event = S3Event {
    _s3eObject :: S3Object
  }
  deriving (Eq, Show)


makeLenses ''S3Object
makeLenses ''S3Event

