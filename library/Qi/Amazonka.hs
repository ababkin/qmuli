{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Qi.Amazonka where

import           Control.Lens                 (set)
import           Control.Monad.Catch          (MonadCatch, MonadThrow)
import           Control.Monad.Trans.AWS      (AWST, runAWST, send)
import           Control.Monad.Trans.Resource (MonadResource, ResourceT)
import           Data.Aeson                   (FromJSON (..), Value (Object),
                                               eitherDecode, (.:))
import           Data.Aeson.Types             (typeMismatch, withObject)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Either                  (either)
import           Network.AWS                  hiding (send)
import           Protolude
import           System.IO                    (stdout)
import           Text.Heredoc


qmuliConfig :: LBS.ByteString
qmuliConfig = [there|./qmuli.json|]

newtype CurrentRegion = CurrentRegion {unCurrentRegion :: Region}
instance FromJSON CurrentRegion where
  parseJSON (Object o) =
    CurrentRegion <$> (withObject "AWS configuration" (.: "region") =<< o .: "aws")
  parseJSON v          = typeMismatch "qmuli configuration" v


currentRegion :: Region
currentRegion = unCurrentRegion $
  either
    (panic . ("could not parse qmuli.yaml: " <>) . toS)
    identity
    $ eitherDecode qmuliConfig


runAmazonka
  :: AWS a
  -> IO a
runAmazonka action = do
  {- logger <- newLogger Debug stdout -}
  {- env <- newEnv Discover <&> set envLogger logger . set envRegion currentRegion -}
  env <- newEnv Discover <&> set envRegion currentRegion
  runResourceT $ runAWST env action

