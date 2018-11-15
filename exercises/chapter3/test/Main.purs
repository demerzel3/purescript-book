module Test.Main where

import Prelude

import Data.AddressBook (
  AddressBook,
  Entry,
  containsName,
  emptyBook,
  findEntry,
  findEntryByStreetAddress,
  insertEntry,
  showEntry
)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (logShow)

entry1 :: Entry
entry1 =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

entry2 :: Entry
entry2 =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "c.so di Porta Ticinese"
             , city: "Milan"
             , state: "MI"
             }
  }

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

main :: Effect Unit
main = do
  let book1 = insertEntry entry2 (insertEntry entry1 emptyBook)

  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1

  logShow $ showEntry <$> findEntryByStreetAddress "123 Fake St." book1

  logShow $ containsName "John" book1
  logShow $ containsName "Jeff" book1

