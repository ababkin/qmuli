{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.RDS.Accessors where

import           Control.Lens
import           Data.Hashable
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.RDS
import           Qi.Config.Identifier



getRdsDbInstanceCFResourceName
  :: RdsDbInstance
  -> Text
getRdsDbInstanceCFResourceName dbInstance =  T.concat [dbInstance^.rdiName, "RDSDBInstance"]


getAllRdsDbInstances
  :: Config
  -> [RdsDbInstance]
getAllRdsDbInstances config = SHM.elems $ config^.rdsConfig.rcDbInstances


getRdsDbInstanceById
  :: RdsDbInstanceId
  -> Config
  -> RdsDbInstance
getRdsDbInstanceById tid = fromJust . SHM.lookup tid . (^.rdsConfig.rcDbInstances)


insertRdsDbInstance
  :: RdsDbInstance
  -> (RdsDbInstanceId, (RdsConfig -> RdsConfig))
insertRdsDbInstance dbInstance = (riid, insertIdToRdsDbInstance)
  where
    insertIdToRdsDbInstance = rcDbInstances %~ SHM.insert riid dbInstance
    riid = RdsDbInstanceId $ hash dbInstance

