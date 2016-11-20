{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Lambda.Interpreters.IO (run) where

import           Control.Lens                 hiding (view)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Operational
import           Control.Monad.Trans.AWS      (AWST, runAWST, send)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson                   (Value (..), encode, object)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Conduit                 as C
import           Data.Conduit.Binary          (sinkLbs)
import           Data.Default                 (def)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8)
import qualified Data.Text.IO                 as T
import           Database.Groundhog.Sqlite
import           Network.AWS                  hiding (send)
import           Network.AWS.DynamoDB         as A
import qualified Network.AWS.S3               as A
import           System.IO                    (stdout)

import           Qi.Amazonka                  (currentRegion)
import           Qi.Config.AWS
import           Qi.Config.AWS.DDB
import           Qi.Config.AWS.DDB.Accessors
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.AWS.S3.Accessors
import           Qi.Config.Identifier         (DdbTableId)
import           Qi.Program.Lambda.Interface  (LambdaInstruction (..),
                                               LambdaProgram)

type QiAWS = AWST (ReaderT Config (ResourceT IO))

run
  :: LambdaProgram ()
  -> Config
  -> IO ()
run program config = do
  env <- newEnv Discover <&> set envRegion currentRegion
  runResourceT . (`runReaderT` config) . runAWST env $ interpret program

  where
    interpret
      :: LambdaProgram ()
      -> QiAWS ()
    interpret program = do
      case view program of
        (GetS3ObjectContent s3Obj) :>>= is -> do
          interpret . is =<< getS3ObjectContent s3Obj

        (PutS3ObjectContent s3Obj content) :>>= is -> do
          interpret . is =<< putS3ObjectContent s3Obj content

        (GetDdbRecord ddbTableId keys) :>>= is -> do
          interpret . is =<< getDdbRecord ddbTableId keys

        (PutDdbRecord ddbTableId item) :>>= is -> do
          interpret . is =<< putDdbRecord ddbTableId item

        (Transactions txs) :>>= is -> do
          interpret . is =<< transactions txs

        (Output content) :>>= is -> do
          output content -- final output, no more program instructions

        Return _ ->
          return def

      where
        {- say :: Show a => Text -> a -> QiAWS () -}
        {- say msg = liftIO . T.putStrLn . mappend msg . T.pack . show -}

        output
          :: BS.ByteString
          -> QiAWS ()
        output content =
          liftIO . LBS.putStr . encode $ object [("body", String $ decodeUtf8 content)]


-- S3

        getS3ObjectContent
          :: S3Object
          -> QiAWS LBS.ByteString
        getS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key objKey} = do
          {- putStrLn $ "GetS3ObjectContent: " ++ show s3Obj -}
          config <- lift ask
          {- say "Reading s3 object content..." "" -}
          r <- send . A.getObject (A.BucketName bucketName) $ A.ObjectKey objKey
          sinkBody (r ^. A.gorsBody) sinkLbs

          where
            bucketName = (bucket^.s3bName) `namePrefixWith` config
            bucket = getS3BucketById _s3oBucketId config

-- TODO: add a streaming version that takes a sink and streams the rsBody into it
-- sinkBody :: MonadResource m => RsBody -> Sink ByteString m a -> m a


        putS3ObjectContent
          :: S3Object
          -> LBS.ByteString
          -> QiAWS ()
        putS3ObjectContent s3Obj@S3Object{_s3oBucketId, _s3oKey = S3Key objKey} content = do
          {- putStrLn $ "PutS3ObjectContent: " ++ show s3Obj -}
          config <- lift ask
          {- say "Writing s3 object content..." "" -}
          r <- send . A.putObject (A.BucketName bucketName) (A.ObjectKey objKey) $ toBody content
          return ()

          where
            bucketName = (bucket^.s3bName) `namePrefixWith` config
            bucket = getS3BucketById _s3oBucketId config



-- DynamoDB

        getDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS DdbAttrs
        getDdbRecord ddbTableId keys = do
          config <- lift ask
          r <- send $ A.getItem tableName & giKey .~ keys
          return $ r ^. A.girsItem

          where
            tableName = (getDdbTableById ddbTableId config)^.dtName


        putDdbRecord
          :: DdbTableId
          -> DdbAttrs
          -> QiAWS ()
        putDdbRecord ddbTableId item = do
          config <- lift ask
          r <- send $ A.putItem tableName & piItem .~ item
          return ()

          where
            tableName = (getDdbTableById ddbTableId config)^.dtName

-- RDS

        transactions
          :: ReaderT Sqlite IO ()
          -> QiAWS ()
        transactions txs = do
          config <- lift ask
          liftIO . withSqliteConn ":memory:" $ runDbConn txs

