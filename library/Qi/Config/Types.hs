{-# LANGUAGE NamedFieldPuns #-}

module Qi.Config.Types where


import           Protolude


data ResourceExistence = AlreadyExists | ShouldCreate
  deriving (Eq, Show)



{- newtype Id r = Id Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON) -}
{- instance FromInt (Id r) where -}
  {- fromInt = Id -}

{- type ResourceIdMap r = HashMap (Id r) r -}

