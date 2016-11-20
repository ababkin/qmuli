{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.CF.RDS (toResources) where

import           Data.Aeson                  (Value (Array), object)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.HashMap.Strict         as SHM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Stratosphere                hiding (name)

import           Qi.Config.AWS
import           Qi.Config.AWS.RDS
import           Qi.Config.AWS.RDS.Accessors


toResources config = Resources . map toRdsDbInstanceRes $ getAllRdsDbInstances config
  where
    toRdsDbInstanceRes inst =
      resource name $
        DBInstanceProperties $
        dbInstance
          dbType
        & dbiDBName ?~ dbName
        & dbiAllocatedStorage ?~ size

      where
        name    = getRdsDbInstanceCFResourceName inst
        dbName  = Literal                 $ inst^.rdiName
        dbType  = Literal . T.pack . show $ inst^.rdiType
        size    = Literal . T.pack . show $ inst^.rdiSize




