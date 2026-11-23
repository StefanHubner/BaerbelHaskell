{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Tools (dispatch, help, helpResult, capabilities) where

import Baerbel.Error (Result, failure)
import Baerbel.JSON (decodeValue, jsonOptions)
import Baerbel.Provider (Execute)
import Data.Aeson hiding (Result)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data Capability = Capability {capName :: Text, capDescription :: Text} deriving (Eq, Show, Generic)
instance ToJSON Capability where toJSON = genericToJSON (jsonOptions 3)
data Schema = Schema {schemaType :: Text, schemaName :: Maybe Text, schemaDescription :: Maybe Text} deriving (Generic)
instance FromJSON Schema where parseJSON = genericParseJSON (jsonOptions 6)
data Help = Help {helpStatus :: Text, helpTools :: [Capability]} deriving (Generic)
instance ToJSON Help where toJSON = genericToJSON (jsonOptions 4)

capabilities :: Map Text Text -> [Value] -> Result [Capability]
capabilities descriptions = traverse parse
  where
    parse value = fmap capability (decodeValue "invalid_tool_definition" value)
    capability schema = Capability (fromMaybe (schemaType schema) (schemaName schema)) description
      where description = fromMaybe (Map.findWithDefault "" (schemaType schema) descriptions) (schemaDescription schema)

help :: Map Text Text -> [Value] -> Execute
help descriptions tools name arguments _ = pure (helpResult descriptions tools name arguments)

helpResult :: Map Text Text -> [Value] -> Text -> Value -> Result Value
helpResult descriptions tools name arguments
  | name /= "assistant_help" = Left (failure "unknown_tool")
  | arguments /= Object mempty = Left (failure "invalid_arguments")
  | otherwise = fmap render (capabilities descriptions tools)
  where render values = toJSON (Help "ok" values)

dispatch :: [(Text, Execute)] -> Execute
dispatch handlers name arguments requestId = maybe missing run selected
  where
    selected = find (\(prefix, _) -> prefix `Text.isPrefixOf` name) handlers
    missing = pure (Left (failure "unknown_tool"))
    run (_, operation) = operation name arguments requestId
