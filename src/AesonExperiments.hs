{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}


module AesonExperiments where

import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither, Parser)
import GHC.Generics
import qualified Data.ByteString.Lazy as LB
import Data.Text

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

value :: Value
value = object [ "first_name" .= ("Juniper" :: String), "last_name" .=( "Lerrad" :: String) ]


data Foo = Foo
  { field1 :: Int
  , field2 :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)
  -- ToJSON so that we can encode *to* a JSON string,
  -- FromJSON so that we can parse *from* a JSON string

jsonString :: LB.ByteString
jsonString = "{ \"field1\": 27, \"field2\": \"hello!\" }"

maybeFoo :: Maybe Foo
maybeFoo = decode jsonString

-- { contact_info: { email: <string> } }
nested :: Value -> Parser String
nested = withObject "ContactInfo" $ \obj -> do
  contact <- obj .: "contact_info"
  contact .: "email"

email = parseMaybe nested $ object
    [ "contact_info" .=
      object [ "email" .= ("williamyaoh@gmail.com" :: String) ]
    ]  

myFoo :: Foo
myFoo = Foo
  { field1 = 909
  , field2 = "take your time"
  }


customValue :: Value
customValue = object
  [ "list_price" .= (150000 :: Int)
  , "sale_price" .= (143000 :: Int)
  , "description" .= ("2-bedroom townhouse" :: String)
  ]

data Person = Person
  { firstName :: String
  , lastName  :: String
  }
  deriving (Show)

-- our fields are snake_case instead
instance ToJSON Person where
  toJSON (Person { firstName = firstName, lastName = lastName }) =
    object [ "first_name" .= firstName
           , "last_name"  .= lastName
           ]

instance FromJSON Person where
  -- note that the typeclass function is parseJSON, not fromJSON
  parseJSON = withObject "Person" $ \obj -> do
    firstName <- obj .: "first_name"
    lastName <- obj .: "last_name"
    return (Person { firstName = firstName, lastName = lastName })

karlJSON :: LB.ByteString
karlJSON = "{\"first_name\":\"Karl\",\"last_name\":\"Popper\"}"

(.->) :: FromJSON a => Parser Object -> Text -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

nested' :: Value -> Parser (String, String)
nested' = withObject "ContactInfo" $ \obj -> do
   email <- obj .: "contact_info" .-> "email"
   state <- obj .: "contact_info" .-> "address" .-> "state"
   return (email, state)



foo = object
    [ "contact_info" .= object
      [ "email" .= ("williamyaoh@gmail.com" :: String)
      , "address" .= object
        [ "state" .= ("OK" :: String)
        , "zip_code" .= ("74008" :: String)
        ]
      ]
   ]


email2 = parseMaybe nested $ object
    [ "contact_info" .=
      object [ "email" .= ("williamyaoh@gmail.com" :: String) ]
    ]  

