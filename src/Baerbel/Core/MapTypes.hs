{-# LANGUAGE DeriveGeneric #-}

module Baerbel.Core.MapTypes (Point(..), Place(..), Coordinate(..), point, placeWaypoint) where

import Baerbel.Core.JSON (camelOptions)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

data Point = Point {latitude :: Double, longitude :: Double} deriving (Eq, Show, Generic)
instance FromJSON Point
instance ToJSON Point

data Place = Place
  { placeId :: Text, placeLabel :: Text, placeAddress :: Text, placePoint :: Point }
  deriving (Eq, Show)

data Coordinate = Coordinate {coordinateLat :: Double, coordinateLng :: Double} deriving (Eq, Show, Generic)
instance FromJSON Coordinate where parseJSON = genericParseJSON (camelOptions 10)
instance ToJSON Coordinate where toJSON = genericToJSON (camelOptions 10)

newtype Waypoint = Waypoint {waypointPlaceId :: Text} deriving (Generic)
instance ToJSON Waypoint where toJSON = genericToJSON (camelOptions 8)

point :: Coordinate -> Point
point value = Point (coordinateLat value) (coordinateLng value)

placeWaypoint :: Place -> Value
placeWaypoint place
  | Text.null (placeId place) = toJSON (CoordinateWaypoint (Position (placePoint place)))
  | otherwise = toJSON (Waypoint (placeId place))

newtype Position = Position {positionLatLng :: Point} deriving (Generic)
instance ToJSON Position where toJSON = genericToJSON (camelOptions 8)
newtype CoordinateWaypoint = CoordinateWaypoint {waypointLocation :: Position} deriving (Generic)
instance ToJSON CoordinateWaypoint where toJSON = genericToJSON (camelOptions 8)
