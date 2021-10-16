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

module API where

import Servant.API
import Servant.HTML.Lucid
import Lucid
import Lucid.HTMX (hx_include_, hx_target_)
import Lucid.HTMX.Safe hiding (hx_include_, hx_target_)
import Servant.Links
import Servant.Server
import Core
import Common


data FormLoginUser = FormLoginUser

instance ToHtml FormLoginUser where
    toHtml FormLoginUser = baseHtml "Login" $ do
        undefined

data FormNewUser = FormNewUser

instance ToHtml FormNewUser where
    toHtml FormNewUser = baseHtml "Create New User" $ do
        undefined

data FormNewCreet = FormNewCreet

instance ToHtml FormNewCreet where
    toHtml FormNewCreet = do
        undefined

data PageHome = PageHome [Creet]
    
instance ToHtml PageHome where
    toHtml (PageHome creets) = baseHtml "Homepage" $ do
        h1_ [] "Homepage"
        br_ []
        p_ [] "Welcome to Crittter"

data PageUserDetail = PageUserDetail AuthorizedUser

instance ToHtml PageUserDetail where
    toHtml (PageUserDetail AuthorizedUser{..}) = baseHtml "Some User" $ do
        undefined

data PageCreetDetail = PageCreetDetail Creet

type Home = Get '[HTML] PageHome

type NewUserForm = "user" :> "new" :> Get '[HTML] FormNewUser

type NewCreetForm = "creet" :> "new" :> Get '[HTML] FormNewCreet

-- TODO: Need auth
type UserDetail = "user" :> Capture "user_id" (ID AuthorizedUser) :> Get '[HTML] PageUserDetail

type CreetDetail = "creet" :> Capture "creet_id" (ID Creet) :> Get '[HTML] PageCreetDetail
