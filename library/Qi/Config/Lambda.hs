{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module Qi.Config.Lambda where

import           Control.Lens
import           Control.Monad.Freer
import           Data.Aeson                           (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Default                         (Default, def)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as SHM
import           Data.Proxy                           (Proxy)
import           Data.Text                            (Text)
import           GHC.Show                             as Show
import           Protolude                            as P
import           Qi.Config.AWS.ApiGw                  (ApiMethodEvent)
import           Qi.Config.AWS.CfCustomResource       (CfCustomResourceLambdaProgram)
import           Qi.Config.AWS.CfCustomResource.Types (CfCustomResourceEvent)
import           Qi.Config.AWS.CW                     (CwEvent, CwLambdaProgram)
import           Qi.Config.AWS.DDB                    (DdbStreamEvent)
import           Qi.Config.Identifier
import           Qi.Config.Resource.S3                (S3Event)
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang                   (S3Eff, S3LambdaProgram)
import           Stratosphere


data LambdaConfig = LambdaConfig {
    _lbdIdToLambda :: HashMap LambdaId Lambda
  , _lbdNameToId   :: HashMap Text LambdaId
  }
  deriving (Eq, Show)
instance Default LambdaConfig where
  def = LambdaConfig {
    _lbdIdToLambda  = SHM.empty
  , _lbdNameToId    = SHM.empty
  }


makeLenses ''LambdaConfig

