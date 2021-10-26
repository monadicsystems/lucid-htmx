{-# LANGUAGE OverloadedStrings #-}

module Lucid.HTMX.Servant where

import Data.Text
import Lucid.Base (Attribute)
import Lucid.HTMX
    ( hxGet_
    , hxPost_
    , hxPut_
    , hxPatch_
    , hxDelete_
    , hxPushUrl_
    )
import Servant.Links (Link)
import Servant.API (toUrlPiece, ToHttpApiData(..))

toUrl :: ToHttpApiData a => a -> Text
toUrl = ("/" <>) . toUrlPiece

hxGetSafe_ :: Link -> Attribute
hxGetSafe_ = hxGet_ . toUrl

hxPostSafe_ :: Link -> Attribute
hxPostSafe_ = hxPost_ . toUrl

hxPutSafe_ :: Link -> Attribute
hxPutSafe_ = hxPut_ . toUrl

hxPatchSafe_ :: Link -> Attribute
hxPatchSafe_ = hxPatch_ . toUrl

hxDeleteSafe_ :: Link -> Attribute
hxDeleteSafe_ = hxDelete_ . toUrl

hxPushUrlSafe_ :: Either Bool Link -> Attribute
hxPushUrlSafe_ boolOrUrl = hxPushUrl_ $ case boolOrUrl of
    Left bool -> if bool then "true" else "false"
    Right url -> toUrl url
