{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Qi.Config.Types where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import           Protolude


data ResourceExistence = AlreadyExists | ShouldCreate
  deriving (Eq, Show)


class FromInt a where
  fromInt :: Int -> a

newtype Id r = Id Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt (Id r) where
  fromInt = Id

type ResourceIdMap r = HashMap (Id r) r

