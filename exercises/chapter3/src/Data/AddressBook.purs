module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- (2) Write a function which looks up an Entry given a street address,
-- by reusing the existing code in findEntry. Test your function in PSCi.

findEntryByStreetAddress :: String -> AddressBook -> Maybe Entry
findEntryByStreetAddress streetAddress book = head $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == streetAddress

-- (3) Write a function which tests whether a name appears in a AddressBook,
-- returning a Boolean value. Hint: Use PSCi to find the type of the Data.List.null function,
-- which test whether a list is empty or not.

containsName :: String -> AddressBook -> Boolean
containsName _ Nil = false
containsName firstName (Cons x xs) = x.firstName == firstName || (containsName firstName xs)
