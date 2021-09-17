{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Monad.IO.Class (liftIO)
import Css3.Selector (csssel)
import qualified Css3.Selector as Css3
import qualified Data.Aeson as Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import GHC.TypeLits
import Hasql.TH
import Lucid
import Lucid.HTMX.Safe
import Network.Wai.Handler.Warp
import Servant.API
import Servant.HTML.Lucid
import Servant.Links
import Servant.Server


newtype ID a = ID { unID :: Int } deriving (Eq, Show, FromHttpApiData, ToHttpApiData)

newtype Email = Email { unEmail :: Text } deriving (Eq, Show, ToHtml)

newtype Name = Name { unName :: Text } deriving (Eq, Show, ToHtml)

data Status = Active | Inactive deriving (Eq, Show)

data Contact = Contact
    { contactID :: ID Contact
    , contactName :: Name
    , contactEmail :: Email
    , contactStatus :: Status
    }
    deriving (Eq, Show)

type ContactTable = Get '[HTML] [Contact]

type DeleteContact = Capture "contact-id" (ID Contact) :> Delete '[HTML] NoContent

-- type AddContact = ...

type API =
    "contacts" :>
        ContactTable :<|> DeleteContact

getContactsFromDB :: IO [Contact]
getContactsFromDB = do
    undefined

deleteContactFromDB :: ID Contact -> IO ()
deleteContactFromDB = do
    undefined

contactTableHandler :: Handler [Contact]
contactTableHandler = liftIO getContactsFromDB

deleteContactHandler :: ID Contact -> Handler NoContent
deleteContactHandler contactID = do
  liftIO $ deleteContactFromDB contactID
  return NoContent

server :: Server API
server = contactTableHandler :<|> deleteContactHandler

contactTableEndpoint :: Proxy ContactTable
contactTableEndpoint = Proxy

deleteContactEndpoint :: Proxy DeleteContact
deleteContactEndpoint = Proxy

api :: Proxy API
api = Proxy

deleteContactLink :: ID Contact -> Link
deleteContactLink contactID = safeLink api deleteContactEndpoint $ contactID

instance ToHtml (ID Contact) where
    toHtml = toHtml . show
    toHtmlRaw = toHtml

instance ToHtml Status where
    toHtml = \case
        Active -> "Active"
        Inactive -> "Inactive"
    toHtmlRaw = toHtml

instance ToHtml Contact where
    toHtml Contact{..} = do
        tr_ [] $ do
            td_ [] $ toHtml contactID
            td_ [] $ toHtml contactName
            td_ [] $ toHtml contactEmail
            td_ [] $ toHtml contactStatus
            td_ [buttonStyle_ "red-500", hx_delete_ $ deleteContactLink contactID] "Delete"
    toHtmlRaw = toHtml

instance ToHtml [Contact] where
    toHtml contacts = do
        table_ [] $ do
            thead_ [] $ do
                tr_ [] $ do
                    th_ [] "Name"
                    th_ [] "Email"
                    th_ [] "Status"
                    th_ [] ""
            tbody_ 
                [ hx_confirm_ "Are you sure?"
                , hx_target_ (HXTargetValSelectorClosest [csssel|tr|])
                , hx_swap_ (HXSwapVal SwapPosOuter (Just $ SwapModDelay 2) Nothing Nothing)
                ]
                (mapM_ toHtml contacts)
    toHtmlRaw = toHtml

main :: IO ()
main = do
    let application = serve @API Proxy server
        port = 8080
    
    print $ "Serving application on port: " <> (show port)
    run port application
