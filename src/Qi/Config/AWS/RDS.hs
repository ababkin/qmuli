{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.Config.AWS.RDS where

import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.Identifier


data RdsInstanceType = PG | MySQL

instance Show RdsInstanceType where
  show PG    = "PostgreSQL"
  show MySQL = "MySQL"


data RdsDbInstance = RdsDbInstance {
    _rdiName :: Text
  , _rdiType :: RdsInstanceType
  , _rdiSize :: Int
  } deriving Show

instance Hashable RdsDbInstance where
  -- TODO: do proper identifiers (random?)
  hashWithSalt s RdsDbInstance{_rdiName} = s `hashWithSalt` _rdiName


data RdsConfig = RdsConfig {
    _rcDbInstances :: HashMap RdsDbInstanceId RdsDbInstance
  } deriving Show

instance Monoid RdsConfig where
  RdsConfig { _rcDbInstances = is1 } `mappend` RdsConfig { _rcDbInstances = is2 } =
    RdsConfig { _rcDbInstances = is1 `mappend` is2 }
  mempty = def

instance Default RdsConfig where
  def = RdsConfig {
    _rcDbInstances = SHM.empty
  }

makeLenses ''RdsDbInstance
makeLenses ''RdsConfig


