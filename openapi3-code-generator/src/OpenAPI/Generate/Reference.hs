{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains function to resolve references within the OpenAPI specification
module OpenAPI.Generate.Reference
  ( ReferenceMap,
    ComponentReference (..),
    SchemaPropertyReference,
    buildReferenceMap,
    getSchemaReference,
    getResponseReference,
    getParameterReference,
    getExampleReference,
    getRequestBodyReference,
    getHeaderReference,
    getSecuritySchemeReference,
    collectSchemaReferences,
  )
where

import Control.Monad
import qualified Data.Bifunctor as BF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS

-- | Represents all types the 'ReferenceMap' can hold
data ComponentReference
  = SchemaReference OAS.SchemaObject
  | ResponseReference OAT.ResponseObject
  | ParameterReference OAT.ParameterObject
  | ExampleReference OAT.ExampleObject
  | RequestBodyReference OAT.RequestBodyObject
  | HeaderReference OAT.HeaderObject
  | SecuritySchemeReference OAT.SecuritySchemeObject
  deriving (Show, Eq, Generic)

-- | A lookup table for references within the OpenAPI specification
type ReferenceMap = Map.Map Text ComponentReference

-- | Creates a 'ReferenceMap' from an 'OAT.OpenApiSpecification' containing all elements within components.
-- It does not capture possibly referenced locations anywhere else in the specification.
buildReferenceMap :: OAT.OpenApiSpecification -> ReferenceMap
buildReferenceMap =
  Map.fromList
    . ( \o ->
          buildReferencesForComponentType "schemas" SchemaReference (OAT.schemas o)
            <> buildReferencesForSchemaProperties (OAT.schemas o)
            <> buildReferencesForComponentType "responses" ResponseReference (OAT.responses (o :: OAT.ComponentsObject))
            <> buildReferencesForComponentType "parameters" ParameterReference (OAT.parameters (o :: OAT.ComponentsObject))
            <> buildReferencesForComponentType "examples" ExampleReference (OAT.examples (o :: OAT.ComponentsObject))
            <> buildReferencesForComponentType "requestBodies" RequestBodyReference (OAT.requestBodies o)
            <> buildReferencesForComponentType "headers" HeaderReference (OAT.headers (o :: OAT.ComponentsObject))
            <> buildReferencesForComponentType "securitySchemes" SecuritySchemeReference (OAT.securitySchemes o)
      )
    . OAT.components

-- | Maps the subtypes of components to the entries of the 'ReferenceMap' and filters references (the lookup table should only contain concrete values).
buildReferencesForComponentType ::
  Text ->
  (a -> ComponentReference) ->
  Map.Map Text (OAT.Referencable a) ->
  [(Text, ComponentReference)]
buildReferencesForComponentType componentName constructor =
  fmap (BF.first (("#/components/" <> componentName <> "/") <>))
    . Maybe.mapMaybe (convertReferencableToReference constructor)
    . Map.toList

convertReferencableToReference ::
  (a -> ComponentReference) ->
  (Text, OAT.Referencable a) ->
  Maybe (Text, ComponentReference)
convertReferencableToReference constructor (name', OAT.Concrete object) = Just (name', constructor object)
convertReferencableToReference _ (_, OAT.Reference _) = Nothing

getReference :: Text -> ReferenceMap -> Maybe ComponentReference
getReference = Map.lookup

createReferenceLookup :: (ComponentReference -> Maybe a) -> Text -> ReferenceMap -> Maybe a
createReferenceLookup conversionFn key = getReference key >=> conversionFn

-- | Resolve a 'OAS.SchemaObject' reference from a 'ReferenceMap'
getSchemaReference :: Text -> ReferenceMap -> Maybe OAS.SchemaObject
getSchemaReference = createReferenceLookup $ \case
  SchemaReference r -> Just r
  _ -> Nothing

-- | Resolve a 'OAT.ResponseObject' reference from a 'ReferenceMap'
getResponseReference :: Text -> ReferenceMap -> Maybe OAT.ResponseObject
getResponseReference = createReferenceLookup $ \case
  ResponseReference r -> Just r
  _ -> Nothing

-- | Resolve a 'OAT.ParameterObject' reference from a 'ReferenceMap'
getParameterReference :: Text -> ReferenceMap -> Maybe OAT.ParameterObject
getParameterReference = createReferenceLookup $ \case
  ParameterReference r -> Just r
  _ -> Nothing

-- | Resolve a 'OAT.ExampleObject' reference from a 'ReferenceMap'
getExampleReference :: Text -> ReferenceMap -> Maybe OAT.ExampleObject
getExampleReference = createReferenceLookup $ \case
  ExampleReference r -> Just r
  _ -> Nothing

-- | Resolve a 'OAT.RequestBodyObject' reference from a 'ReferenceMap'
getRequestBodyReference :: Text -> ReferenceMap -> Maybe OAT.RequestBodyObject
getRequestBodyReference = createReferenceLookup $ \case
  RequestBodyReference r -> Just r
  _ -> Nothing

-- | Resolve a 'OAT.HeaderObject' reference from a 'ReferenceMap'
getHeaderReference :: Text -> ReferenceMap -> Maybe OAT.HeaderObject
getHeaderReference = createReferenceLookup $ \case
  HeaderReference r -> Just r
  _ -> Nothing

-- | Resolve a 'OAT.SecuritySchemeObject' reference from a 'ReferenceMap'
getSecuritySchemeReference :: Text -> ReferenceMap -> Maybe OAT.SecuritySchemeObject
getSecuritySchemeReference = createReferenceLookup $ \case
  SecuritySchemeReference r -> Just r
  _ -> Nothing

getParametersFromOperation :: OAT.OperationObject -> [OAT.Referencable OAT.ParameterObject]
getParametersFromOperation = OAT.parameters

type SchemaPropertyReference = (Text, Text, Bool)

collectSchemaReferences :: OAT.OpenApiSpecification -> Set.Set SchemaPropertyReference
collectSchemaReferences spec = 
        collectFromPaths (Map.elems $ OAT.paths spec)
        `Set.union` collectFromSchemas (Map.elems $ OAT.schemas $ OAT.components spec)
  where
    collectFromSchemas :: [OAT.Schema] -> Set.Set SchemaPropertyReference
    collectFromSchemas schemas =
      let concrete = Set.unions [collectFromSchema s | OAT.Concrete s <- schemas]
          references = Set.fromList $ Maybe.catMaybes [parseSchemaReference ref | OAT.Reference ref <- schemas]
      in  concrete `Set.union` references

    collectFromSchema :: OAS.SchemaObject -> Set.Set SchemaPropertyReference
    collectFromSchema schema = Set.unions [
        collectFromSchemas $ Map.elems (OAS.properties schema),
        collectFromSchemas $ OAS.allOf schema,
        collectFromSchemas $ OAS.oneOf schema,
        collectFromSchemas $ OAS.anyOf schema,
        collectFromSchemas $ Maybe.maybeToList $ OAS.items schema
      ]

    collectFromPaths :: [OAT.PathItemObject] -> Set.Set SchemaPropertyReference
    collectFromPaths paths = Set.unions $ map collectFromPath paths

    collectFromPath :: OAT.PathItemObject -> Set.Set SchemaPropertyReference
    collectFromPath po =
      Set.unions [collectFromOperations (method po) | method <- [OAT.get, OAT.put, OAT.post, OAT.delete, OAT.options, OAT.head, OAT.patch, OAT.trace]]

    collectFromOperations :: Maybe OAT.OperationObject -> Set.Set SchemaPropertyReference
    collectFromOperations Nothing = Set.empty
    collectFromOperations (Just op) =
        Set.fromList (Maybe.catMaybes [fromParameterReference p | p <- getParametersFromOperation op])
        `Set.union` collectFromRequest (OAT.requestBody op)
        `Set.union` collectFromResponse (OAT.responses (op :: OAT.OperationObject))

    collectFromRequest :: Maybe (OAT.Referencable OAT.RequestBodyObject) -> Set.Set SchemaPropertyReference
    collectFromRequest (Just (OAT.Concrete body)) = Set.unions $ map collectFromMediaType $ Map.elems $ getRequestBodyContent body
    collectFromRequest _ = Set.empty

    collectFromResponse :: OAT.ResponsesObject -> Set.Set SchemaPropertyReference
    collectFromResponse rs = Set.unions $ map collectFromResponseObject $ concat [
          Maybe.maybeToList (getResponsesObjectDefault rs),
          Maybe.maybeToList (OAT.range1XX rs),
          Maybe.maybeToList (OAT.range2XX rs),
          Maybe.maybeToList (OAT.range3XX rs),
          Maybe.maybeToList (OAT.range4XX rs),
          Maybe.maybeToList (OAT.range5XX rs),
          Map.elems (OAT.perStatusCode rs)
        ]

    collectFromResponseObject :: OAT.Referencable OAT.ResponseObject -> Set.Set SchemaPropertyReference
    collectFromResponseObject (OAT.Concrete rs) =
      Set.unions $ map collectFromMediaType $ Map.elems $ getResponseObjectContent rs
    collectFromResponseObject _ = Set.empty

    collectFromMediaType :: OAT.MediaTypeObject -> Set.Set SchemaPropertyReference
    collectFromMediaType m =
      case getMediaTypeSchema m of
        Nothing -> Set.empty
        Just schema -> collectFromSchemas [schema]

    getMediaTypeSchema :: OAT.MediaTypeObject -> Maybe OAT.Schema
    getMediaTypeSchema = OAT.schema

    getRequestBodyContent :: OAT.RequestBodyObject -> Map.Map Text OAT.MediaTypeObject
    getRequestBodyContent = OAT.content

    getResponseObjectContent :: OAT.ResponseObject -> Map.Map Text OAT.MediaTypeObject
    getResponseObjectContent = OAT.content

    getResponsesObjectDefault :: OAT.ResponsesObject -> Maybe (OAT.Referencable OAT.ResponseObject)
    getResponsesObjectDefault = OAT.default'

    fromParameterReference :: OAT.Referencable OAT.ParameterObject -> Maybe SchemaPropertyReference
    fromParameterReference (OAT.Reference ref) = parseSchemaReference ref
    fromParameterReference _ = Nothing

    parseSchemaReference :: Text -> Maybe SchemaPropertyReference
    parseSchemaReference ref =
      case T.splitOn "/" ref of
        ["#", "components", "schemas", schemaName, "properties", propertyName] -> Just (schemaName, propertyName, False)
        ["#", "components", "schemas", schemaName, "properties", propertyName, "items"] -> Just (schemaName, propertyName, True)
        _ -> Nothing

buildReferencesForSchemaProperties :: Map.Map Text OAS.Schema -> [(Text, ComponentReference)]
buildReferencesForSchemaProperties schemas = concatMap listSchemaProperties (Map.toList schemas)
  where
    listSchemaProperties :: (Text, OAS.Schema) -> [(Text, ComponentReference)]
    listSchemaProperties (schemaName, OAT.Concrete schema) = Maybe.mapMaybe (propertyReference schemaName) (Map.toList $ OAS.properties schema)
    listSchemaProperties _ = []

    propertyReference :: Text -> (Text, OAS.Schema) -> Maybe (Text, ComponentReference)
    propertyReference schemaName (propertyName, OAT.Concrete propertySchema) =
        Just ("#/components/schemas/" <> schemaName <> "/properties/" <> propertyName, SchemaReference propertySchema)
    propertyReference _ _ = Nothing

