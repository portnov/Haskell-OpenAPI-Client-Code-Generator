{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Functionality to Generate Haskell Code out of an OpenAPI definition File
module OpenAPI.Generate.Main where

import Control.Monad
import qualified Data.Bifunctor as BF
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified OpenAPI.Common as OC
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Unknown
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Model as Model
import qualified OpenAPI.Generate.ModelDependencies as Dep
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Operation as Operation
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.SecurityScheme as SecurityScheme
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS
import qualified OpenAPI.Generate.Reference as Ref

-- | Defines all the operations as functions and the common imports
defineOperations :: String -> OAT.OpenApiSpecification -> OAM.Generator (Q [Dep.ModuleDefinition], Dep.Models)
defineOperations moduleName specification =
  let paths = Map.toList $ OAT.paths specification
   in OAM.nested "paths" $ do
        warnAboutUnknownOperations paths
        fmap
          ( BF.bimap
              ( fmap concat
                  . sequence
              )
              Set.unions
          )
          . mapAndUnzipM (uncurry $ Operation.defineOperationsForPath moduleName)
          $ paths

-- | Defines the @defaultURL@ and the @defaultConfiguration@ containing this URL.
defineConfigurationInformation :: String -> OAT.OpenApiSpecification -> Q Doc
defineConfigurationInformation moduleName spec =
  let servers' = (OAT.servers :: OAT.OpenApiSpecification -> [OAT.ServerObject]) spec
      defaultURL = getServerURL servers'
      defaultURLName = mkName "defaultURL"
      getServerURL = maybe "/" (OAT.url :: OAT.ServerObject -> Text) . Maybe.listToMaybe
      defaultApplicationNameVarName = mkName "defaultApplicationName"
      defaultApplicationName = OAT.title $ OAT.info spec
   in Doc.addConfigurationModuleHeader moduleName
        . vcat
        <$> sequence
          [ pure $
              Doc.generateHaddockComment
                [ "The default url specified by the OpenAPI specification",
                  "",
                  "@" <> defaultURL <> "@"
                ],
            ppr
              <$> [d|$(varP defaultURLName) = T.pack $(stringE $ T.unpack defaultURL)|],
            pure $
              Doc.generateHaddockComment
                [ "The default application name used in the @User-Agent@ header which is based on the @info.title@ field of the specification",
                  "",
                  "@" <> defaultApplicationName <> "@"
                ],
            ppr
              <$> [d|$(varP defaultApplicationNameVarName) = T.pack $(stringE $ T.unpack defaultApplicationName)|],
            pure $ Doc.generateHaddockComment ["The default configuration containing the 'defaultURL' and no authorization"],
            ppr <$> [d|$(varP $ mkName "defaultConfiguration") = OC.Configuration $(varE defaultURLName) OC.anonymousSecurityScheme True $(varE defaultApplicationNameVarName)|]
          ]

-- | Defines all models in the components.schemas section of the 'OAT.OpenApiSpecification'
defineModels :: String -> OAT.OpenApiSpecification -> Dep.Models -> [Ref.SchemaPropertyReference] -> OAM.Generator (Q [Dep.ModuleDefinition])
defineModels moduleName spec operationDependencies referencedProperties =
  let schemaDefinitions = Map.toList $ OAT.schemas $ OAT.components spec
   in OAM.nested "components" $
        OAM.nested "schemas" $ do
          warnAboutUnknownWhiteListedOrOpaqueSchemas schemaDefinitions
          schemaModels <- mapM (uncurry Model.defineModelForSchema) schemaDefinitions
          OAM.logWarning $ "References: " <> T.intercalate ", " [schemaName <> "." <> propertyName <> "/" <> T.pack (show t) | (schemaName, propertyName, t) <- referencedProperties]
          let exposedProps = getExposedSchemaProperties referencedProperties schemaDefinitions
          OAM.logWarning $ "Exposed: " <> T.intercalate ", " [schemaName <> "." <> propertyName <> "/" <> T.pack (show t) | (schemaName, propertyName, t, schema) <- exposedProps]
          propertyModels <- mapM (Model.defineModelForSchemaProperty) exposedProps
          let models = schemaModels ++ propertyModels
          whiteListedSchemas <- OAM.getSetting OAO.settingWhiteListedSchemas
          let dependencies = Set.union operationDependencies $ Set.fromList $ fmap transformToModuleName whiteListedSchemas
          OAM.logWarning $ "Dependencies: " <> T.intercalate ", " (Set.toList dependencies)
          OAM.logWarning $ "Models: " <> T.intercalate ", " (map fst models)
          pure $ Dep.getModelModulesFromModelsWithDependencies moduleName dependencies models

getExposedSchemaProperties :: [Ref.SchemaPropertyReference] -> [(Text,OAT.Schema)] -> [(Text, Text, Bool, OAT.Schema)]
getExposedSchemaProperties references schemas =
  let propertyMap = foldr (\(schemaName, propertyName, isItems) m -> Map.insertWith (++) schemaName [(propertyName, isItems)] m) Map.empty references

      processSchema :: (Text, OAT.Schema) -> [(Text, Text, Bool, OAT.Schema)]
      processSchema (schemaName, OAT.Concrete schema) =
          Maybe.catMaybes [formatSchema schemaName propertyName isItems $ Map.lookup propertyName (OAS.properties schema) | (propertyName, isItems) <- Map.findWithDefault [] schemaName propertyMap]
      processSchema _ = []

      formatSchema :: Text -> Text -> Bool -> Maybe OAT.Schema -> Maybe (Text, Text, Bool, OAT.Schema)
      formatSchema schemaName propertyName False (Just s) = Just (schemaName, propertyName, False, s)
      formatSchema schemaName propertyName True (Just (OAT.Concrete s)) =
        case OAS.items s of
          Just items -> Just (schemaName, propertyName, True, items)
          Nothing -> Nothing
      formatSchema _ _ _ _ = Nothing

  in  concatMap processSchema schemas

-- | Defines all supported security schemes from the 'OAT.OpenApiSpecification'.
defineSecuritySchemes :: String -> OAT.OpenApiSpecification -> OAM.Generator (Q Doc)
defineSecuritySchemes moduleName =
  OAM.nested "components"
    . fmap (fmap $ Doc.addSecuritySchemesModuleHeader moduleName)
    . SecurityScheme.defineSupportedSecuritySchemes (T.pack moduleName)
    . Maybe.mapMaybe
      ( \(name', scheme') -> case scheme' of
          OAT.Concrete s -> Just (name', s)
          OAT.Reference _ -> Nothing
      )
    . Map.toList
    . OAT.securitySchemes
    . OAT.components
