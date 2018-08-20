{-# LANGUAGE OverloadedStrings #-}

module Qi.CLI.Dispatcher (
    invokeLambda
  , updateLambdas
  , deployApp
  , createCfStack
  , updateCfStack
  , describeCfStack
  , destroyCfStack
  , cycleStack
  , renderCfTemplate
  , lambdaLogs
  ) where

import           Control.Lens
import           Control.Monad.Freer           (send)
import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.Text                     as T
import           Network.AWS                   (AWS)
import           Protolude                     hiding (FilePath, getAll)
import           System.Environment.Executable (splitExecutablePath)
import           Turtle                        (FilePath, fromString, toText)

import qualified Qi.Amazonka                   as A
import           Qi.CLI.Dispatcher.Build       (build)
import           Qi.CLI.Dispatcher.CF          (createStack, deleteStack,
                                                describeStack, updateStack,
                                                waitOnStackCreated,
                                                waitOnStackDeleted,
                                                waitOnStackUpdated)
import qualified Qi.CLI.Dispatcher.Lambda      as Lambda
import           Qi.CLI.Dispatcher.S3          (clearBuckets)
import           Qi.Config.AWS                 (Config, getAll, getPhysicalName,
                                                namePrefix)
import           Qi.Config.AWS.Lambda          (Lambda)
import           Qi.Config.AWS.S3              (S3Key (S3Key), s3Object)
import qualified Qi.Config.CfTemplate          as CfTemplate
import           Qi.Program.Config.Lang        (getConfig)
import qualified Qi.Program.S3.Lang            as S3
import qualified Qi.Program.Wiring.IO          as IO
import           Qi.Util                       (printPending, printSuccess)


type Dispatcher = ReaderT Config IO

withConfig
  :: (Config -> Dispatcher ())
  -> Dispatcher ()
withConfig action = action =<< ask

withAppName
  :: (Text -> Dispatcher ())
  -> Dispatcher ()
withAppName action = withConfig $ action . (^.namePrefix)

runAmazonka
  :: AWS a
  -> Dispatcher a
runAmazonka = liftIO . A.runAmazonka


invokeLambda
  :: Text
  -> ReaderT Config IO ()
invokeLambda name = Lambda.invoke name =<< liftIO getLine

updateLambdas :: Dispatcher ()
updateLambdas = withConfig $ \config -> do
  let appName = config ^. namePrefix

      allLambdas :: [Lambda]
      allLambdas = getAll config

  printSuccess "updating the lambdas..."
  runAmazonka . Lambda.update appName $ map (getPhysicalName config) allLambdas


lambdaLogs
  :: Text
  -> ReaderT Config IO ()
lambdaLogs = const pass -- runAmazonka . Lambda.logs


renderCfTemplate :: Dispatcher ()
renderCfTemplate =
   withConfig $ liftIO . LBS.putStr . CfTemplate.render

deployApp :: Dispatcher ()
deployApp =
  withConfig $ \config -> do
    let appName = config ^. namePrefix

    printSuccess "deploying the app..."
    content <- liftIO $ do
      (_, execFilename) <- splitExecutablePath -- get the current executable filename
      lambdaPackagePath <- fromString <$> build "." (toS execFilename)
      LBS.readFile . toS $ toTextIgnore lambdaPackagePath

    liftIO $ IO.run "dispatcher" config $ do
      bucketId <- S3.createBucket appName
      send (print bucketId :: IO ())
      conf <- getConfig
      send (print conf :: IO ())
      S3.putContent (s3Object bucketId $ S3Key "cf.json") $ CfTemplate.render config -- TODO: render this inside docker container: https://github.com/qmuli/qmuli/issues/60
      send (print ("done cf.json" :: Text) :: IO ())
      S3.putContent (s3Object bucketId $ S3Key "lambda.zip") content

  where
    toTextIgnore :: FilePath -> T.Text
    toTextIgnore x = case toText x of
      Right s -> s
      Left _  -> ""


createCfStack :: Dispatcher ()
createCfStack =
  withAppName $ \appName -> do
    printSuccess "creating the stack..."
    runAmazonka $ createStack appName
    printPending "waiting on the stack to be created..."
    liftIO $ waitOnStackCreated appName
    printSuccess "stack was successfully created"


updateCfStack :: Dispatcher ()
updateCfStack =
  withAppName $ \appName -> do
    printSuccess "updating the stack..."
    runAmazonka $ updateStack appName
    printPending "waiting on the stack to be updated..."
    liftIO $ waitOnStackUpdated appName
    -- TODO: make lambda updating concurrent with the above stack update?
    updateLambdas
    printSuccess "stack was successfully updated"

describeCfStack :: Dispatcher ()
describeCfStack =
  withAppName $ liftIO . LBS.putStrLn . encodePretty
                  <=< runAmazonka . describeStack


destroyCfStack
  :: Dispatcher ()
  -> Dispatcher ()
destroyCfStack action =
  withConfig $ \config -> do
    let appName = config ^. namePrefix

    printSuccess "destroying the stack..."

    liftIO $ IO.run "dispatcher" config $ clearBuckets config

    runAmazonka $ do
      deleteStack appName

    action

    printPending "waiting on the stack to be destroyed..."
    liftIO $ waitOnStackDeleted appName
    printSuccess "stack was successfully destroyed"


cycleStack :: Dispatcher ()
cycleStack = do
    destroyCfStack $
      deployApp
    createCfStack
    printSuccess "all done!"



