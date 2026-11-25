module Baerbel.Core.HTTP where

import Baerbel.Core.Error ( Failure(..), Result, failure )
import Baerbel.Core.JSON ( decodeBytes )
import Data.Aeson ( Value(..) )
import Data.ByteString ( ByteString )
import Data.Text ( Text )
import qualified Data.Text as Text ( pack )

data Body = EmptyBody | JsonBody Value | FormBody [(ByteString, ByteString)]
  deriving (Eq, Show)

data Request = Request
  { requestMethod :: ByteString
  , requestUrl :: Text
  , requestHeaders :: [(ByteString, ByteString)]
  , requestQuery :: [(ByteString, Maybe ByteString)]
  , requestBody :: Body
  , requestLimit :: Int
  , requestTimeout :: Int
  } deriving (Eq, Show)

data Response = Response
  { responseStatus :: Int, responseHeaders :: [(ByteString, ByteString)]
  , responseBody :: ByteString } deriving (Eq, Show)

responseStatusResult :: (Int -> Bool) -> Int -> Result ()
responseStatusResult mutation status
  | status >= 200 && status < 300 = Right ()
  | otherwise = Left (Failure ("http_" <> Text.pack (show status)) uncertain)
  where uncertain = mutation status

decodeBody :: ByteString -> Result Value
decodeBody bytes = decodeBytes "invalid_response" bytes >>= jsonObject

jsonObject :: Value -> Result Value
jsonObject value@(Object _) = Right value
jsonObject _ = Left (failure "invalid_response")
