{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import           Control.Lens                hiding (view)
import           Control.Monad               (void)
import           Data.Aeson
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.HashMap.Strict         as SHM
import           Data.Maybe                  (fromJust)
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Groundhog
import           Database.Groundhog.TH

import           Qi                          (withConfig)
import           Qi.Config.AWS.Api           (ApiEvent (..),
                                              ApiVerb (Get, Post),
                                              RequestBody (..), aeBody,
                                              aeParams, rpPath)
import           Qi.Config.AWS.RDS           (RdsInstanceType (PG))
import           Qi.Config.Identifier        (S3BucketId)
import           Qi.Program.Config.Interface (ConfigProgram)
import qualified Qi.Program.Config.Interface as CI
import           Qi.Program.Lambda.Interface (LambdaProgram, output,
                                              transactions)

-- Use the two curl commands below to test-drive the two endpoints (substitute your unique api stage url first):
--
-- curl -v -X POST -H "Content-Type: application/json" -d "{\"S\": \"hello there\"}" "https://2gezp5kxjb.execute-api.us-east-1.amazonaws.com/v1/things/xyz"
-- curl -v -X GET "https://2gezp5kxjb.execute-api.us-east-1.amazonaws.com/v1/things/xyz"


data Customer = Customer {
    customerName :: String
  , phone        :: String
  } deriving Show
data Product = Product {
    productName :: String
  , quantity    :: Int
  , customer    :: DefaultKey Customer
  }
deriving instance Show Product

mkPersist defaultCodegenConfig [groundhog|
- entity: Customer               # Name of the datatype
  constructors:
    - name: Customer
      fields:
        - name: customerName
          # Set column name to "name" instead of "customerName"
          dbName: name
      uniques:
        - name: NameConstraint
          fields: [customerName] # Inline format of list
- entity: Product
|]

main :: IO ()
main =
  "apigwlambda" `withConfig` config

    where
      config :: ConfigProgram ()
      config = do
        api     <- CI.api "world"
        things  <- CI.apiRootResource "things" api
        thing   <- CI.apiChildResource "{thingId}" things

        db <- CI.rdsDbInstance "myDB" PG 1

        void $ CI.apiMethodLambda
          "putProp"
          Post
          thing
          $ putPropLambda


      putPropLambda
        :: ApiEvent
        -> LambdaProgram ()
      putPropLambda event@ApiEvent{} = do
        transactions $ do
          runMigration $ do
            migrate (undefined :: Customer)
            migrate (undefined :: Product)
          void . insert $ Customer "John Doe" "0123456789"

        output "successfully added item"

