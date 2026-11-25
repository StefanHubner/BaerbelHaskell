{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Core.Tools (help, capabilities) where

import Baerbel.Core.Error (Result)
import Baerbel.Core.JSON (decodeValue, jsonOptions)
import Data.Aeson hiding (Result)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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

help :: Map Text Text -> [Value] -> Result Value
help descriptions tools = render <$> capabilities descriptions tools
  where render values = toJSON (Help "ok" values)
