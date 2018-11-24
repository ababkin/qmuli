{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Qi.Config.Id where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import           Protolude

data Resource =
    S3Bucket
  | Lambda
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ResourceId (r :: Resource) where
  S3BucketId    :: ResourceId 'S3Bucket
  LambdaId      :: ResourceId 'Lambda

class HasId (r :: Resource) where
  getId :: Proxy r -> Id r

instance HasId 'S3Bucket where
  getId _ = S3BucketId

instance HasId 'Lambda where
  getId _ = LambdaId

{- class FromInt a where -}
  {- fromInt :: Int -> a -}

{- newtype Id r = Id Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON) -}
{- instance FromInt (Id r) where -}
  {- fromInt = Id -}
