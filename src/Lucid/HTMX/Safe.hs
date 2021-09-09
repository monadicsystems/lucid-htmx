{-# LANGUAGE OverloadedStrings #-}

module Lucid.HTMX.Safe where

import Lucid.Base
import Lucid.HTMX.Base as Base
import Data.Set (Set)
import Data.Set as Set
import Data.Text (Text)
import Data.Text as Text
-- import Servant.Links (Link)
-- import Servant.Links as Links

type Link = Text -- Should represent a type-safe link from a library like servant

-- | Makes hx_boost_ a "boolean attribute" since the only valid value for hx-boost is "true".
hx_boost_ :: Attribute
hx_boost_ = Base.hx_boost_ "true"

hx_confirm_ :: Text -> Attribute
hx_confirm_ = Base.hx_confirm_

hx_delete_ :: Link -> Attribute
hx_delete_ = undefined

hx_disable_ :: Attribute
hx_disable_ = Base.hx_disable_

-- | Makes hx_encoding_ a "boolean attribute" since the only valid value for hx-encoding is "multipart/form-data".
hx_encoding_ :: Attribute
hx_encoding_ = Base.hx_encoding_ "multipart/form-data"

data HTMXExtension =
    JSONEnc
    | MethodOverride
    | MorphdomSwap
    | ClientSideTemplates
    | Debug
    | PathDeps
    | ClassTools
    | RemoveMe
    | IncludeVals
    | AJAXHeader
    | EventHeader
    | Preload
    | Other Text
    deriving (Eq)

instance Show HTMXExtension where
    show :: HTMXExtenstion -> String
    show htmlExt = case htmlExt of
        JSONEnc -> "json-enc"
        MethodOverride -> "method-override"
        MorphdomSwap -> "morphdom-swap"
        ClientSideTemplates -> "client-side-templates"
        Debug -> "debug"
        PathDeps -> "path-deps"
        ClassTools -> "class-tools"
        RemoveMe -> "remove-me"
        IncludedVals -> "included-vals"
        AJAXHeader -> "ajax-header"
        EventHeader -> "event-header"
        Preload -> "preload"
        Other extName -> Text.unpack extName

data HTMXExtensions =
    HTMXExtensions (Set HtmxExtension)
    | IgnoreHTMXExtensions (Set HTMXExtensions)
    deriving (Eq, Show)

hx_ext_ :: HTMXExtensions -> Attribute
hx_ext_ htmxExts = case htmxExts of
    HtmxExtensions htmxExtSet -> htmxExtSetToText htmxExtSet
    IgnoreHTMXExtensions htmxExtSet' -> "ignore:" <> (htmlExtSetToText htmxExtSet')
    where
        htmxExtensionsToText :: Set HTMLExtension -> Text
        htmxExtensionsToText htmlExtSet'' = case S.toList htmlExtSet'' of
            [] -> ""
            htmxExtList -> Text.intersperse ',' $ map (Text.pack . show) htmxExtsList

