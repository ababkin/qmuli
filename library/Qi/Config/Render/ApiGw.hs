{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Qi.Config.Render.ApiGw where
{-
import           Data.Aeson                                  (Value (Bool),
                                                              object)
import qualified Data.ByteString.Lazy                        as LBS
import qualified Data.HashMap.Strict                         as SHM
import qualified Data.Text                                   as T
import           Protolude
import           Qi.Config.AWS
import           Qi.Config.AWS.ApiGw
import qualified Qi.Config.AWS.ApiGw.ApiAuthorizer.Accessors as ApiAuthorizer
import qualified Qi.Config.AWS.ApiGw.ApiDeployment.Accessors as ApiDeployment
import qualified Qi.Config.AWS.ApiGw.ApiMethod.Accessors     as ApiMethod
import           Qi.Config.AWS.ApiGw.ApiMethod.Profile       (ampAuthId)
import qualified Qi.Config.AWS.ApiGw.ApiResource.Accessors   as ApiResource
import qualified Qi.Config.AWS.Lambda.Accessors              as Lambda
import           Qi.Config.Identifier
import           Stratosphere                                hiding (Delete)
import           Stratosphere.Types                          (AuthorizerType (..))
import           Text.Heredoc


toResources :: Config -> Resources
toResources config = Resources . foldMap toStagedApiResources $ getAllWithIds config

  where
    jsonContentType = "application/json"

    toStagedApiResources :: (ApiId, Api) -> [Resource]
    toStagedApiResources (apiid, api) = deploymentResource:apiResources
      where
        apiResources = [ apiRes ]
          <> map toApiAuthorizers (ApiAuthorizer.getChildren apiid config)
          <> foldMap toApiChildResources (ApiResource.getChildren (Left apiid) config)

        apiLName = getLogicalName config api

        apiRes = (
          resource apiLName $
            ApiGatewayRestApiProperties $
              apiGatewayRestApi
              & agraName ?~ Literal (api ^. aName)
          )

        deploymentResource :: Resource
        deploymentResource = (
            resource name $
              ApiGatewayDeploymentProperties $
                apiGatewayDeployment (Ref apiLName)
                & agdStageName ?~ "v1"
          )
          & resourceDependsOn ?~ deps

          where
            name = ApiDeployment.getLogicalName api
            deps = map (^. resourceName) apiResources


        toApiAuthorizers :: ApiAuthorizerId -> Resource
        toApiAuthorizers apiaid = (
          resource name $
            ApiGatewayAuthorizerProperties $
              apiGatewayAuthorizer (Ref apiLName)
              & agaProviderARNs ?~ [userPoolArn]
              & agaName ?~ Literal (auth^.aaName)
              & agaType ?~ Literal COGNITO_USER_POOLS_AUTH
              & agaIdentitySource ?~ "method.request.header.Authorization"
          )
          {- & resourceDependsOn ?~ [cognitoLName] -}

          where
            userPoolArn = Join "" [
                "arn:aws:cognito-idp:"
              , Ref "AWS::Region"
              , ":"
              , Ref "AWS::AccountId"
              , ":userpool/"
              , userPoolPhysicalId
              ]
            userPoolPhysicalId = GetAtt cognitoLName "UserPoolId"

            name = getLogicalName config $ getById config apiaid
            auth = getById config apiaid
            cognito = getById config (auth^.aaCognitoId)
            cognitoLName = getLogicalName config cognito



        toApiChildResources :: ApiResourceId -> [Resource]
        toApiChildResources arid =
          [ apirResource ] ++
          methodResources ++
          foldMap toApiChildResources (ApiResource.getChildren (Right arid) config)

          where
            apir = getById config arid
            apirLName = getLogicalName config apir

            apirResource =
              case apir of
                ApiResource{_arParent = Left aid} ->
                  let
                    apiParent = GetAtt apiLName "RootResourceId"
                  in
                    resource apirLName $
                      ApiGatewayResourceProperties $
                      apiGatewayResource
                        apiParent
                        (Literal $ apir ^. arName)
                        (Ref apiLName)


                ApiResource{_arParent = Right arid'} ->
                  let
                    apirParentLName = getLogicalNameFromId config arid'
                  in
                    resource apirLName $
                      ApiGatewayResourceProperties $
                      apiGatewayResource
                        (Ref apirParentLName)
                        (Literal $ apir ^. arName)
                        (Ref apiLName)



            methodResources = [ corsMethodResource ] ++ map toMethodResource (apir ^. arMethodConfigs)

            corsMethodResource =
                resource name $
                  ApiGatewayMethodProperties $
                  apiGatewayMethod
                    (Literal OPTIONS)
                    (Ref apirLName)
                    (Ref apiLName)
                    & agmeAuthorizationType ?~ Literal NONE
                    & agmeIntegration ?~ integration
                    & agmeMethodResponses ?~ [ methodResponse ]

              where
                name = ApiMethod.getLogicalName apir Options

                methodResponse =
                  apiGatewayMethodMethodResponse
                    "200"
                    & agmmrResponseModels ?~ responseModels
                    & agmmrResponseParameters ?~ responseParams

                  where
                    responseModels = [(jsonContentType, "Empty")]

                    responseParams = [
                        ("method.response.header.Access-Control-Allow-Headers", Bool False)
                      , ("method.response.header.Access-Control-Allow-Methods", Bool False)
                      , ("method.response.header.Access-Control-Allow-Origin", Bool False)
                      ]


                integration =
                  apiGatewayMethodIntegration
                  & agmiType ?~ Literal MOCK
                  & agmiIntegrationHttpMethod ?~ Literal POST -- looks like this should always be "POST"
                  & agmiPassthroughBehavior ?~ Literal WHEN_NO_MATCH
                  & agmiRequestTemplates ?~ requestTemplates
                  & agmiIntegrationResponses ?~ [ integrationResponse ]

                  where
                    requestTemplates = [ (jsonContentType, "{\"statusCode\": 200}") ]

                    integrationResponse =
                      apiGatewayMethodIntegrationResponse
                        "200"
                        & agmirResponseTemplates ?~ responseTemplates
                        & agmirResponseParameters ?~ responseParams

                      where
                        responseTemplates = [ (jsonContentType, "") ]
                        responseParams = [
                            ("method.response.header.Access-Control-Allow-Headers", "'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'")
                          , ("method.response.header.Access-Control-Allow-Methods", "'DELETE,GET,HEAD,POST,PUT,OPTIONS,TRACE'")
                          , ("method.response.header.Access-Control-Allow-Origin", "'*'")
                          ]


            toMethodResource ApiMethodConfig{amcVerb, amcLbdId, amcProfile} = (
                resource name $
                  ApiGatewayMethodProperties $
                  specAuth $
                  apiGatewayMethod
                    (Literal $ verb amcVerb)
                    (Ref apirLName)
                    (Ref apiLName)
                    & agmeIntegration ?~ integration
                    & agmeMethodResponses ?~ methodResponses

              )
              & resourceDependsOn ?~ [
                    lbdLName
                  , lbdPermLName
                  ]

              where
                specAuth res = case amcProfile^.ampAuthId of
                  Nothing ->
                    res
                    & agmeAuthorizationType ?~ Literal NONE

                  Just authId ->
                    let
                      authLName = getLogicalNameFromId config authId
                    in
                    res
                    & agmeAuthorizationType ?~ Literal COGNITO_USER_POOLS
                    & agmeAuthorizerId ?~ Ref authLName

                name = ApiMethod.getLogicalName apir amcVerb

                verb Get     = GET
                verb Post    = POST
                verb Put     = PUT
                verb Delete  = DELETE
                verb Head    = HEAD
                verb Options = OPTIONS

                -- these are all possible response types (statuses) for this method
                methodResponses = map methodResponse ["200", "400", "404", "500"]
                  where

                    methodResponse status =
                      apiGatewayMethodMethodResponse
                        status
                        & agmmrResponseParameters ?~ responseParams


                    responseParams = [
                        ("method.response.header.Access-Control-Allow-Headers", Bool False)
                      , ("method.response.header.Access-Control-Allow-Methods", Bool False)
                      , ("method.response.header.Access-Control-Allow-Origin", Bool False)
                      ]

                lbdId = getById config amcLbdId
                lbdLName = getLogicalName config lbdId
                lbdPermLName = Lambda.getPermissionLogicalName config lbdId

                integration =
                  apiGatewayMethodIntegration
                    & agmiType ?~ Literal AWS
                    & agmiIntegrationHttpMethod ?~ Literal POST -- looks like this should always be "POST" no matter what the http verb was used on the endpoint
                    & agmiUri ?~ uri
                    & agmiPassthroughBehavior ?~ passthroughBehavior
                    & agmiRequestTemplates ?~ requestTemplates
                    & agmiIntegrationResponses ?~ integrationResponses

                  where
                    uri = (Join "" [
                        "arn:aws:apigateway:"
                      , Ref "AWS::Region"
                      , ":lambda:path/2015-03-31/functions/"
                      , GetAtt lbdLName "Arn"
                      , "/invocations"])

                    requestTemplates =
                      -- TODO: all the same for now. Need to figure out how it should differ for different verbs
                      [ (jsonContentType, postTemplate) ]
                      {- case amcVerb of -}
                        {- Get  -> [ (jsonContentType, postTemplate) ] -}
                        {- Post -> [ (jsonContentType, postTemplate) ] -}

                      where
                        {- getTemplate   = [there|./js/get_template.js|] -}
                        postTemplate  = [there|./js/post_template.js|]

                    passthroughBehavior =
                      -- TODO: all the same for now. Need to figure out how it should differ for different verbs
                      Literal WHEN_NO_TEMPLATES
                      {- case amcVerb of -}
                        {- Get  -> WHEN_NO_TEMPLATES -}
                        {- Post -> WHEN_NO_TEMPLATES -}


                    integrationResponses = [ successIntegrationResponse ] ++
                      map errorIntegrationResponse [
                          "400"
                        , "404"
                        , "500"
                        ]

                    successIntegrationResponse =
                      apiGatewayMethodIntegrationResponse
                        "200"
                        & agmirResponseTemplates ?~ responseTemplates
                        & agmirResponseParameters ?~ responseParams

                      where
                        responseTemplates = [ (jsonContentType, [there|./js/success_response_template.js|]) ]

                    errorIntegrationResponse errorStatus =
                      apiGatewayMethodIntegrationResponse
                        (Literal $ T.pack errorStatus)
                        & agmirResponseTemplates ?~ responseTemplates
                        & agmirSelectionPattern ?~ (Literal . T.pack $ "^\\[" ++ errorStatus ++ "\\].*")
                        & agmirResponseParameters ?~ responseParams

                      where
                        responseTemplates = [ (jsonContentType, [there|./js/error_response_template.js|]) ]

                    responseParams = [
                        ("method.response.header.Access-Control-Allow-Headers", "'Content-Type,X-Amz-Date,Authorization,X-Api-Key'")
                      , ("method.response.header.Access-Control-Allow-Methods", "'DELETE,GET,HEAD,POST,PUT,OPTIONS,TRACE'")
                      , ("method.response.header.Access-Control-Allow-Origin", "'*'")
                      ]

toOutputs
  :: Config
  -> Outputs
toOutputs config =
  Outputs . map toApiOutput $ getAllWithIds config

  where
    toApiOutput
      :: (ApiId, Api)
      -> Output
    toApiOutput (_, api) =
      output (T.concat [apiLName, "URL"])
        apiUrl
        & outputDescription ?~ "RestApi URL"

      where
        -- https://{restapi_id}.execute-api.{region}.amazonaws.com/{stage_name}/
        apiUrl = Join "" [
            "https://"
          , Ref apiLName
          , ".execute-api.us-east-1.amazonaws.com/v1"
          ]

        apiLName = getLogicalName config api

-}
