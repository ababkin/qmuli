{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qi.Config.Render.KF (toResources) where

import           Control.Lens
import           Protolude        hiding (getAll)
import           Qi.Config.AWS
import           Qi.Config.AWS.KF
import           Qi.Config.Types  (ResourceExistence (AlreadyExists))
import           Stratosphere     (ResourceProperties (KinesisFirehoseDeliveryStreamProperties),
                                   Resources (Resources), Val (GetAtt, Literal))
import qualified Stratosphere     as S (kfdsDeliveryStreamName,
                                        kfdsDeliveryStreamType,
                                        kfdsExtendedS3DestinationConfiguration,
                                        kfdsKinesisStreamSourceConfiguration,
                                        kfdsS3DestinationConfiguration,
                                        kinesisFirehoseDeliveryStream,
                                        kinesisFirehoseDeliveryStreamS3DestinationConfiguration,
                                        resource, resourceDependsOn)


toResources
  :: Config
  -> Resources
toResources config = Resources $ toResource <$> kfs
  where
    kfs = getAll config

    toResource (kf :: Kf) = (
      S.resource (unLogicalName lname) $
        KinesisFirehoseDeliveryStreamProperties $ S.kinesisFirehoseDeliveryStream
          & S.kfdsDeliveryStreamName ?~ Literal (unPhysicalName pname)
          & S.kfdsDeliveryStreamType ?~ Literal "DirectPut"
          & S.kfdsS3DestinationConfiguration ?~ s3Dest
          {- & S.kfdsExtendedS3DestinationConfiguration ?~ lbdConfigs -}
          {- & S.kfdsKinesisStreamSourceConfiguration ?~ -}
      )
      {- & S.resourceDependsOn ?~ reqs -}

      where
        lname = getLogicalName config kf
        pname = getPhysicalName config kf

        s3Dest = S.kinesisFirehoseDeliveryStreamS3DestinationConfiguration
                  bucketARNarg
                  bufferingHintsarg
                  compressionFormatarg
                  roleARNarg
                  {- & kfdssdcCloudWatchLoggingOptions ?~ -}
                  {- & kfdssdcEncryptionConfiguration ?~ -}
                  {- & kfdssdcPrefix ?~ -}

        bucketARNarg = undefined
        bufferingHintsarg = undefined
        compressionFormatarg = undefined
        roleARNarg = undefined
