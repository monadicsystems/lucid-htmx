{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
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
import Data.Functor.Identity (Identity)
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
import Lucid.HTMX (hx_include_, hx_target_)
import Lucid.HTMX.Safe hiding (hx_include_, hx_target_)
import Network.Wai.Handler.Warp
import Prelude
import Servant.API
import Servant.HTML.Lucid
import Servant.Links
import Servant.Server

import qualified Css3.Selector as Css3
import qualified Data.Aeson as Aeson
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- import Database
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection


main :: IO ()
main = undefined

-- MODEL START --

-- | A genre for books, like sci-fi, fantasy, romance, etc.
newtype Genre = Genre { unGenre :: Text } deriving (Eq, Show)

-- newtype Email = Email { unEmail :: Text } deriving (Eq, Show)

-- | Represents a book
data Book = Book
    { bookTitle :: Text
    , bookAuthor :: Author
    , bookGenres :: [Genre]
    , bookPages :: Int
    }
    deriving (Eq, Show)

-- | Represents an author
data Author = Author
    { authorFirstName :: Text
    , authorLastName :: Text
    }
    deriving (Eq, Show)

-- | A newtype wrapper for an ID of some type
newtype ID a = ID { unID :: Int } deriving (Eq, Show)

-- | Represents a data that has an ID attached to it
data HasID a = HasID
    { hasIDID :: ID a
    , hasIDData :: a
    }
    deriving (Eq, Show)

-- MODEL END --

-- TEMPLATES START --

data TemplateLibrary = TemplateLibrary [Book]

instance ToHtml TemplateLibrary where

data TemplateAuthors = TemplateAuthors [Author]

instance ToHtml TemplateAuthors where

data TemplateBookDetail = TemplateBookDetail Book

instance ToHtml TemplateBookDetail where

data TemplateAuthorDetail = TemplateAuthorDetail Author [Book]

instance ToHtml TemplateAuthorDetail where

-- TEMPLATES END --

-- ROUTES START --

-- | Route for users personal library
type Library = "library" :> Get '[HTML] TemplateLibrary

-- | Get detailed info about book
type GetBook = "library" :> Capture "bookID" (ID Book) :> Get '[HTML] TemplateBookDetail

-- | Get all authors that have written the books in personal library
type GetAuthors = "authors" :> Get '[HTML] TemplateAuthors

-- | Get detailed info about author
type GetAuthor = "authors" :> Capture "authorID" (ID Author) :> Get '[HTML] TemplateAuthorDetail

-- | Add a book to your personal library
type AddBook = "library" :> ReqBody '[JSON] Book :> Post '[HTML] TemplateLibrary

-- | Add an author to your personal library
type AddAuthor = "authors" :> ReqBody '[JSON] Author :> Post '[HTML] TemplateAuthors

-- | Delete book from personal library
type DeleteBook = "library" :> Capture "bookID" (ID Book) :> Delete '[HTML] NoContent

-- | Delete an author and all their books from personal library
type DeleteAuthor = "authors" :> Capture "authorID" (ID Author) :> Delete '[HTML] NoContent

type Routes =
    Library
    :<|> GetBook
    :<|> GetAuthors
    :<|> GetAuthor
    :<|> AddBook
    :<|> AddAuthor
    :<|> DeleteBook
    :<|> DeleteAuthor

-- ROUTES END --

-- DATABASE START --

dropBooksTable :: Session ()
dropBooksTable = Session.sql
    [uncheckedSql|
        drop table if exists books
        |]

dropAuthorsTable :: Session ()
dropAuthorsTable = Session.sql
    [uncheckedSql|
        drop table if exists books
        |]

createBooksTable :: Session ()
createBooksTable = Session.sql
    [uncheckedSql|
        create table if not exists contacts (
            id serial primary key,
            name varchar (50) unique not null,
            email varchar (255) unique not null,
            status varchar (10) not null
        );
        |]

createAuthorsTable :: Session ()
createAuthorsTable = Session.sql
    [uncheckedSql|
        create table if not exists contacts (
            id serial primary key,
            name varchar (50) unique not null,
            email varchar (255) unique not null,
            status varchar (10) not null
        );
        |]

insertBook :: Statement Book (HasID Book)
insertBook =
    dimap
        contactFormToTuple
        tupleToContact
        [singletonStatement|
            insert into contacts (name, email, status)
            values ($1 :: text, $2 :: text, $3 :: text)
            returning id :: int4, name :: text, email :: text, status :: text
        |]
    where
        contactFormToTuple :: ContactForm -> (Text, Text, Text)
        contactFormToTuple ContactForm{..} = (unName contactFormName, unEmail contactFormEmail, Text.pack . show $ contactFormStatus)

        tupleToContact :: (Int32, Text, Text, Text) -> Contact
        tupleToContact (id, name, email, status) = Contact
            { contactID = ID id
            , contactName = Name name
            , contactEmail = Email email
            , contactStatus = read . Text.unpack $ status
            }

insertBooks :: Statement [Book] ()
insertBooks =
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

insertAuthors :: Statement [Author] ()
insertAuthors =
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

selectBook :: Statement (ID Book) (HasID Book)
selectBook =
    dimap
        unID
        tupleToContact
        [singletonStatement|
            select id :: int4, name :: text, email :: text, status :: text
            from contacts
            where id = $1 :: int4
        |]
    where
        tupleToContact :: (Int32, Text, Text, Text) -> Contact
        tupleToContact (id, name, email, status) = Contact
            { contactID = ID id
            , contactName = Name name
            , contactEmail = Email email
            , contactStatus = read . Text.unpack $ status
            }

selectAuthor :: Statement (ID Author) (HasID Author)
selectAuthor =
    dimap
        unID
        tupleToContact
        [singletonStatement|
            select id :: int4, name :: text, email :: text, status :: text
            from contacts
            where id = $1 :: int4
        |]
    where
        tupleToContact :: (Int32, Text, Text, Text) -> Contact
        tupleToContact (id, name, email, status) = Contact
            { contactID = ID id
            , contactName = Name name
            , contactEmail = Email email
            , contactStatus = read . Text.unpack $ status
            }

selectBooks :: Statement () [Contact] -- Add sort arg
selectBooks =
    dimap id (Vector.toList . fmap tupleToContact)
        [vectorStatement|
            select id :: int4, name :: text, email :: text, status :: text
            from contacts
            |]
    where
        tupleToContact :: (Int32, Text, Text, Text) -> Contact
        tupleToContact (id, name, email, status) = Contact
            { contactID = ID id
            , contactName = Name name
            , contactEmail = Email email
            , contactStatus = read . Text.unpack $ status
            }

selectAuthors :: Statement () [Contact] -- Add sort arg
selectAuthors =
    dimap id (Vector.toList . fmap tupleToContact)
        [vectorStatement|
            select id :: int4, name :: text, email :: text, status :: text
            from contacts
            |]
    where
        tupleToContact :: (Int32, Text, Text, Text) -> Contact
        tupleToContact (id, name, email, status) = Contact
            { contactID = ID id
            , contactName = Name name
            , contactEmail = Email email
            , contactStatus = read . Text.unpack $ status
            }

deleteBook :: Statement (ID Book) ()
deleteBook =
    dimap (\(ID contactID) -> contactID) id
        [resultlessStatement| 
            delete from contacts where id = $1 :: int4
            |]

deleteAuthor :: Statement (ID Author) ()
deleteAuthor =
    dimap (\(ID contactID) -> contactID) id
        [resultlessStatement| 
            delete from contacts where id = $1 :: int4
            |]

updateContactStatement :: Statement (ID Contact, ContactForm) Contact
updateContactStatement =
    dimap
        contactFormWithIDToTuple
        tupleToContact
        [singletonStatement|
            update contacts
            set name = $2 :: Text,
                email = $3 :: Text,
                status = $4 :: Text
            where id = $1 :: int4
            returning id :: int4, name :: text, email :: text, status :: text
        |]
    where
        contactFormWithIDToTuple :: (ID Contact, ContactForm) -> (Int32, Text, Text, Text)
        contactFormWithIDToTuple (contactID, ContactForm{..}) =
            (unID contactID, unName contactFormName, unEmail contactFormEmail, Text.pack . show $ contactFormStatus)

        tupleToContact :: (Int32, Text, Text, Text) -> Contact
        tupleToContact (id, name, email, status) = Contact
            { contactID = ID id
            , contactName = Name name
            , contactEmail = Email email
            , contactStatus = read . Text.unpack $ status
            }

insertContactDB :: Connection.Connection -> ContactForm -> IO Contact
insertContactDB conn contactForm = do
    Right res <- Session.run (Session.statement contactForm insertContactStatement) conn
    pure res

insertContactsDB :: Connection.Connection -> [ContactForm] -> IO ()
insertContactsDB conn contacts = do
    Right res <- Session.run (Session.statement contacts insertContactsStatement) conn
    pure res

getContactFromDB :: Connection.Connection -> ID Contact -> IO Contact
getContactFromDB conn contactID = do
    Right res <- Session.run (Session.statement contactID getContactStatement) conn
    pure res

getContactsFromDB :: Connection.Connection -> IO [Contact]
getContactsFromDB conn = do
    Right res <- Session.run (Session.statement () getContactsStatement) conn
    pure res

deleteContactFromDB :: Connection.Connection -> ID Contact -> IO ()
deleteContactFromDB conn contactID = do
    Right res <- Session.run (Session.statement contactID deleteContactStatement) conn
    pure res

updateContactDB :: Connection.Connection -> (ID Contact, ContactForm) -> IO Contact
updateContactDB conn contactFormWithID = do
    Right res <- Session.run (Session.statement contactFormWithID updateContactStatement) conn
    pure res

-- DATABASE END --

-- HANDLERS START --



-- HANDLERS END --

-- HANDLERS END --

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
                application = serve @(API (Identity ())) Proxy $ server conn
            
            print $ "Serving application on port: " <> (show port)
            run port application
