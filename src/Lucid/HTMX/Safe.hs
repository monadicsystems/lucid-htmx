{-# LANGUAGE InstanceSigs #-}
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
    show :: HTMXExtension -> String
    show htmlExt = case htmlExt of
        JSONEnc -> "json-enc"
        MethodOverride -> "method-override"
        MorphdomSwap -> "morphdom-swap"
        ClientSideTemplates -> "client-side-templates"
        Debug -> "debug"
        PathDeps -> "path-deps"
        ClassTools -> "class-tools"
        RemoveMe -> "remove-me"
        IncludeVals -> "included-vals"
        AJAXHeader -> "ajax-header"
        EventHeader -> "event-header"
        Preload -> "preload"
        Other extName -> Text.unpack extName

data HTMXExtensions =
    HTMXExtensions (Set HTMXExtension)
    | IgnoreHTMXExtensions (Set HTMXExtension)
    deriving (Eq, Show)

hx_ext_ :: HTMXExtensions -> Attribute
hx_ext_ htmxExts = Base.hx_ext_ $ case htmxExts of
    HTMXExtensions htmxExtSet -> htmxExtSetToText htmxExtSet
    IgnoreHTMXExtensions htmxExtSet' -> "ignore:" <> (htmxExtSetToText htmxExtSet')
    where
        htmxExtSetToText :: Set HTMXExtension -> Text
        htmxExtSetToText htmxExtSet'' = case Set.toList htmxExtSet'' of
            [] -> ""
            htmxExtList -> Text.intercalate "," $ Prelude.map (Text.pack . show) htmxExtList

hx_get_ :: Link -> Text
hx_get_ = undefined

-- Placeholder until I can bring in aeson...
type JSON = Text

class ToJSON a where
    toJSON :: a -> JSON

class FromJSON a where
    fromJSON :: JSON -> a

newtype JavaScript = JavaScript { unJavaScript :: Text } deriving (Eq, Show)

hx_headers_ :: ToJSON a => a -> Attribute
hx_headers_ = undefined

hx_history_elt_ :: Attribute
hx_history_elt_ = Base.hx_history_elt_

data QuerySelector =
    QuerySelectorTag Text
    | QuerySelectorDot Text
    | QuerySelectorHash Text
    | QuerySelectorAttribute Text Text
    deriving (Eq, Show)

hx_include_ :: QuerySelector -> Attribute
hx_include_ = undefined

data HXIndicatorSelector = 
    HXIndicatorSelector QuerySelector
    | HXIndicatorSelectorClosest QuerySelector

hx_indicator_ :: HXIndicatorSelector -> Attribute
hx_indicator_ = undefined

data HXParams =
    HXParamsAll
    | HXParamsNone
    | HXParamsNotList (Set Text)
    | HXParamsList (Set Text)
    deriving (Eq, Show)

hx_params_ :: HXParams -> Attribute
hx_params_ = undefined

hx_patch_ :: Link -> Attribute
hx_patch_ = undefined

hx_post_ :: Link -> Attribute
hx_post_ = undefined

-- For same reasons as hx_boost_
hx_preserve_ :: Attribute
hx_preserve_ = Base.hx_preserve_ "true"

hx_prompt_ :: Text -> Attribute
hx_prompt_ = Base.hx_prompt_

hx_push_url_ :: Link -> Attribute
hx_push_url_ = undefined

hx_put_ :: Link -> Attribute
hx_put_ = undefined

-- Still needs more research below
type HXRequest = Text

hx_request_ :: HXRequest -> Attribute
hx_request_ = undefined

hx_select_ :: QuerySelector -> Attribute
hx_select_ = undefined

-- More research
type HXSSE = Text

hx_sse_ :: HXSSE -> Attribute
hx_sse_ = undefined

type HXSwapOOB = Text

hx_swap_oob_ :: HXSwapOOB -> Attribute
hx_swap_oob_ = undefined

data HXSwap =
    HXSwapInner
    | HXSwapOuter
    | HXSwapBeforeBegin
    | HXSwapAfterBegin
    | HXSwapBeforeEnd
    | HXSwapAfterEnd
    | HXSwapNone
    deriving (Eq, Show)

hx_swap_ :: HXSwap -> Attribute
hx_swap_ = undefined

-- More research

