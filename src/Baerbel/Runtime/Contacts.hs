{-# LANGUAGE OverloadedStrings #-}
module Baerbel.Runtime.Contacts (module Baerbel.Core.Contacts, contacts) where

import Baerbel.Core.Contacts
import Baerbel.Core.Error ( Result, failure )
import Baerbel.Core.JSON ( decodeValue )
import Baerbel.Core.ProviderConfig ( ContactsRules(..) )
import Baerbel.Runtime.Cache ( onceSuccess )
import Baerbel.Runtime.Effects ( continue )
import Baerbel.Runtime.HTTP ( Send, requestJSON )
import Baerbel.Runtime.Provider ( Execute )
import Control.Concurrent ( threadDelay )

warm :: ContactsRules -> Send -> IO (Result ())
warm rules send = do
  let request = lookupContacts rules "" 1
  response <- requestJSON send request (decodeValue "contacts_invalid_response")
  continue settle response
  where
    delay = contactsWarmupSeconds rules * 1000000
    settle :: Matches -> IO (Result ())
    settle _ = threadDelay delay >> pure (Right ())

contacts :: ContactsRules -> Send -> IO Execute
contacts rules send = do
  ready <- onceSuccess (warm rules send)
  pure (execute rules send ready)

execute :: ContactsRules -> Send -> IO (Result ()) -> Execute
execute rules send ready name arguments _
  | name /= "contacts_search" = pure (Left (failure "contacts_unknown_tool"))
  | otherwise = continue perform prepared
  where
    prepared = decodeValue "contacts_invalid_arguments" arguments >>= prepareSearch rules
    perform query = ready >>= continue (const (retrieve query))
    retrieve query = requestJSON send request parse
      where
        requested = searchLimit query + 1
        request = lookupContacts rules (searchText query) requested
        parse = fmap (search rules query) . decodeValue "contacts_invalid_response"
