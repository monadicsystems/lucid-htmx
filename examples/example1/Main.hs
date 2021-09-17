{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Contravariant.Extras.Contrazip (contrazip3)
import Control.Monad.IO.Class (liftIO)
import Css3.Selector (csssel)
import Data.Int
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Proxy
import Data.Text
import Data.Tuple.Curry
import Data.Vector (Vector)
import GHC.Generics
import GHC.TypeLits
import Hasql.TH
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import Lucid
import Lucid.HTMX.Safe
import Network.Wai.Handler.Warp
import Prelude
import Servant.API
import Servant.HTML.Lucid
import Servant.Links
import Servant.Server

import qualified Css3.Selector as Css3
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection


newtype ID a = ID { unID :: Int32 } deriving (Eq, Show, FromHttpApiData, ToHttpApiData)

newtype Email = Email { unEmail :: Text } deriving (Eq, Show, ToHtml)

newtype Name = Name { unName :: Text } deriving (Eq, Show, ToHtml)

data Status = Active | Inactive deriving (Eq, Show, Read)

data Contact = Contact
    { contactID :: ID Contact
    , contactName :: Name
    , contactEmail :: Email
    , contactStatus :: Status
    }
    deriving (Eq, Show)

data ContactForm = ContactForm
    { contactFormName :: Name
    , contactFormEmail :: Email
    , contactFormStatus :: Status
    }
    deriving (Eq, Show)

type ContactTable = Get '[HTML] [Contact]

type DeleteContact = Capture "contact-id" (ID Contact) :> Delete '[HTML] NoContent

-- type AddContact = ...

type API = ContactTable :<|> DeleteContact

dropContactsSession :: Session ()
dropContactsSession = Session.sql
    [uncheckedSql|
        drop table if exists contacts
        |]

createContactsSession :: Session ()
createContactsSession = Session.sql
    [uncheckedSql|
        create table if not exists contacts (
            id serial primary key,
            name varchar (50) unique not null,
            email varchar (255) unique not null,
            status varchar (10) not null
        );
        |]
    -- TODO: created_on TIMESTAMP NOT NULL

insertContactsStatement :: Statement [ContactForm] ()
insertContactsStatement =
    dimap
        contactFormsUnzip
        id
        [resultlessStatement|
            insert into contacts (name, email, status)
            select * from unnest ($1 :: text[], $2 :: text[], $3 :: text[])
        |]
    where
        contactFormsUnzip :: [ContactForm] -> (Vector Text, Vector Text, Vector Text)
        contactFormsUnzip =
            Vector.unzip3
            . fmap
                (\ContactForm{..} -> 
                    (unName contactFormName, unEmail contactFormEmail, Text.pack . show $ contactFormStatus)
                )
            . Vector.fromList

getContactsStatement :: Statement () [Contact]
getContactsStatement =
    dimap id (Vector.toList . fmap tupleToContact)
        [vectorStatement|
            select id :: int4, name :: text, email :: text, status :: text
            from "contacts"
            |]
    where
        tupleToContact :: (Int32, Text, Text, Text) -> Contact
        tupleToContact (id, name, email, status) = Contact
            { contactID = ID id
            , contactName = Name name
            , contactEmail = Email email
            , contactStatus = read . Text.unpack $ status
            }

deleteContactStatement :: Statement (ID Contact) ()
deleteContactStatement =
    dimap (\(ID contactID) -> contactID) id
        [resultlessStatement| 
            delete from contacts where id = $1 :: int4
            |]

insertContactsDB :: Connection.Connection -> [ContactForm] -> IO ()
insertContactsDB conn contacts = do
    Right res <- Session.run (Session.statement contacts insertContactsStatement) conn
    pure res

getContactsFromDB :: Connection.Connection -> IO [Contact]
getContactsFromDB conn = do
    Right res <- Session.run (Session.statement () getContactsStatement) conn
    pure res

deleteContactFromDB :: Connection.Connection -> ID Contact -> IO ()
deleteContactFromDB conn contactID = do
    Right res <- Session.run (Session.statement contactID deleteContactStatement) conn
    pure res

contactTableHandler :: Connection.Connection -> Handler [Contact]
contactTableHandler conn = liftIO $ getContactsFromDB conn

deleteContactHandler :: Connection.Connection -> ID Contact -> Handler NoContent
deleteContactHandler conn contactID = do
  liftIO $ deleteContactFromDB conn contactID
  return NoContent

server :: Connection.Connection -> Server API
server conn = contactTableHandler conn :<|> deleteContactHandler conn

contactTableEndpoint :: Proxy ContactTable
contactTableEndpoint = Proxy

deleteContactEndpoint :: Proxy DeleteContact
deleteContactEndpoint = Proxy

api :: Proxy API
api = Proxy

deleteContactLink :: ID Contact -> Link
deleteContactLink contactID = safeLink api deleteContactEndpoint $ contactID

instance ToHtml (ID Contact) where
    toHtml = toHtml . show . unID
    toHtmlRaw = toHtml

instance ToHtml Status where
    toHtml = \case
        Active -> "Active"
        Inactive -> "Inactive"
    toHtmlRaw = toHtml

tableCellStyle_ color = class_ $ "border-4 border-blue-400 items-center justify-center px-4 py-2 bg-"<>color

tableButtonStyle_ color =
    classes_ ["px-4", "py-2", "bg-red-500", "text-lg", "text-white", "rounded-md", "bg-"<>color]

instance ToHtml Contact where
    toHtml Contact{..} = do
        tr_ [] $ do
            td_ [tableCellStyle_ "green-300", class_ " text-semibold text-lg "] $ toHtml contactID
            td_ [tableCellStyle_ "green-300", class_ " text-semibold text-lg "] $ toHtml contactName
            td_ [tableCellStyle_ "green-300", class_ " text-semibold text-lg "] $ toHtml contactEmail
            td_ [tableCellStyle_ "green-300", class_ " text-semibold text-lg "] $ toHtml contactStatus
            td_ [tableCellStyle_ "green-300", class_ " text-semibold text-lg "] $ do
                span_ [class_ "flex flex-row justify-center align-middle"] $ do
                    button_ [tableButtonStyle_ "pink-400", class_ " mr-2 ", hx_delete_ $ deleteContactLink contactID] "Edit"
                    button_ [tableButtonStyle_ "red-400", hx_delete_ $ deleteContactLink contactID] "Delete"
    toHtmlRaw = toHtml

instance ToHtml [Contact] where
    toHtml contacts = baseHtml "Contact Table" $ do
        div_ [class_ "flex items-center justify-center h-screen"] $ do
            table_ [class_ "table-auto rounded-lg"] $ do
                thead_ [] $ do
                    tr_ [] $ do
                        th_ [tableCellStyle_ "yellow-200", class_ " text-lg "] "ID"
                        th_ [tableCellStyle_ "yellow-200", class_ " text-lg "] "Name"
                        th_ [tableCellStyle_ "yellow-200", class_ " text-lg "] "Email"
                        th_ [tableCellStyle_ "yellow-200", class_ " text-lg "] "Status"
                        th_ [tableCellStyle_ "yellow-200", class_ " text-lg "] "Action(s)"
                tbody_ 
                    [ hx_confirm_ "Are you sure?"
                    , hx_target_ (HXTargetValSelectorClosest [csssel|tr|])
                    , hx_swap_ (HXSwapVal SwapPosOuter (Just $ SwapModDelay 2) Nothing Nothing)
                    ]
                    $ do
                        (Prelude.mapM_ toHtml contacts)
                        tr_ [] $ do
                            td_ [tableCellStyle_ "green-300"] ""
                            td_ [tableCellStyle_ "green-300"] $ input_ [class_ "rounded-md px-2", type_ "text"]
                            td_ [tableCellStyle_ "green-300"] $ input_ [class_ "rounded-md px-2", type_ "text"]
                            td_ [tableCellStyle_ "green-300"] $ do
                                span_ [class_ "flex flex-row justify-center align-middle"] $ do
                                    p_ [class_ "mr-2 text-lg"] "Active?"
                                    input_ [class_ "rounded-md my-auto h-4 w-6", type_ "checkbox"]
                            td_ [tableCellStyle_ "green-300"] $
                                button_
                                    [ tableButtonStyle_ "purple-400"
                                    , class_ " w-full "
                                    -- , hx_delete_ $ deleteContactLink contactID
                                    ]
                                    "Add"

    toHtmlRaw = toHtml

main :: IO ()
main = do
    let dbConnSettings = Connection.settings "localhost" 5432 "postgres" "dummy" "ex1"
        initialContacts =
            [ ContactForm (Name "Alice Jones") (Email "alice@gmail.com") Active
            , ContactForm (Name "Bob Hart") (Email "bhart@gmail.com") Inactive
            , ContactForm (Name "Corey Smith") (Email "coreysm@grubco.com") Active
            ]

    connResult <- Connection.acquire dbConnSettings
    case connResult of
        Left err -> print err
        Right conn -> do
            Session.run dropContactsSession conn
            Session.run createContactsSession conn
            insertContactsDB conn initialContacts

            let port = 8080
                application = serve @API Proxy $ server conn
            
            print $ "Serving application on port: " <> (show port)
            run port application