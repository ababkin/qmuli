{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.Config.AWS.KF where

import           Control.Lens
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Default         (Default, def)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           GHC.Show             (Show (..))
import           Protolude
import           Qi.Config.Identifier
import           Qi.Config.Types


data KfConfig = KfConfig {
    _kfIdToKf :: HashMap KfId Kf
  }
  deriving (Eq, Show)
instance Default KfConfig where
  def = KfConfig {
      _kfIdToKf     = SHM.empty
    }

data Kf = Kf {
    _kfName    :: Text
  , _kfProfile :: KfProfile
  }
  deriving (Eq, Show)
instance Default Kf where
  def = Kf {
      _kfName         = "default"
    , _kfProfile      = def
    }

data KfProfile = KfProfile
  { _kfpExistence    :: ResourceExistence
  }
  deriving (Eq, Show)
instance Default KfProfile where
  def = KfProfile
    { _kfpExistence = ShouldCreate
    }

makeLenses ''KfConfig
makeLenses ''Kf
makeLenses ''KfProfile
