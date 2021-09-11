{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.HTMX.Safe where

import qualified Css3.Selector as CssSelector
import Css3.Selector (ToCssSelector)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import Lucid.Base (Attribute)
import qualified Lucid.HTMX.Base as Base
import qualified Servant.API as Servant
import Servant.API (ToHttpApiData(..))
import Servant.Links (Link)
import qualified Servant.Links as Servant


-- | Makes hx_boost_ a "boolean attribute" since the only valid value for hx-boost is "true".
hx_boost_ :: Attribute
hx_boost_ = Base.hx_boost_ "true"

hx_confirm_ :: Text -> Attribute
hx_confirm_ = Base.hx_confirm_

hx_delete_ :: Link -> Attribute
hx_delete_ = Base.hx_delete_ . Servant.toUrlPiece

hx_disable_ :: Attribute
hx_disable_ = Base.hx_disable_

-- | Makes hx_encoding_ a "boolean attribute" since the only valid value for hx-encoding is "multipart/form-data".
hx_encoding_ :: Attribute
hx_encoding_ = Base.hx_encoding_ "multipart/form-data"

data HTMXExt =
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

instance Show HTMXExt where
    show :: HTMXExt -> String
    show htmxExt = case htmxExt of
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

type HXExtArg = Set HTMXExt

hx_ext_ :: HXExtArg -> Attribute
hx_ext_ = Base.hx_ext_ . Text.intercalate "," . Prelude.map (Text.pack . show) . Set.toList

hx_ext_ignore_ :: HXExtArg -> Attribute
hx_ext_ignore_ = Base.hx_ext_ . ("ignore:" <>) . Text.intercalate "," . Prelude.map (Text.pack . show) . Set.toList

hx_get_ :: Link -> Attribute
hx_get_ = Base.hx_get_ . Servant.toUrlPiece

-- newtype JavaScript = JavaScript { unJavaScript :: Text } deriving (Eq)

-- instance Show JavaScript where
    -- show :: JavaScript -> String
    -- show (JavaScript unJS) = "javascript:" <> show unJS

-- | Value of hx_headers_ must be valid JSON
hx_headers_ :: ToJSON a => a -> Attribute
hx_headers_ = Base.hx_headers_ . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

hx_history_elt_ :: Attribute
hx_history_elt_ = Base.hx_history_elt_

hx_include_ :: ToCssSelector a => a -> Attribute
hx_include_ = Base.hx_include_ . CssSelector.toCssSelector

hx_indicator_ :: ToCssSelector a => a -> Attribute
hx_indicator_ = Base.hx_indicator_ . CssSelector.toCssSelector

hx_indicator_closest_ :: ToCssSelector a => a -> Attribute
hx_indicator_closest_ = Base.hx_indicator_ . ("closest " <>) . CssSelector.toCssSelector

type HXParamArg = [Text]

hx_params_ :: HXParamArg -> Attribute
hx_params_ = Base.hx_params_ . Text.intercalate ","

hx_params_not_ :: HXParamArg -> Attribute
hx_params_not_ = Base.hx_params_ . ("not " <>) . Text.intercalate ","

hx_params_all_ :: Attribute
hx_params_all_ = Base.hx_params_ "*"

hx_params_none_ :: Attribute
hx_params_none_ = Base.hx_params_ "none"

hx_patch_ :: Link -> Attribute
hx_patch_ = Base.hx_patch_ . Servant.toUrlPiece

hx_post_ :: Link -> Attribute
hx_post_ = Base.hx_post_ . Servant.toUrlPiece

-- For same reasons as hx_boost_
hx_preserve_ :: Attribute
hx_preserve_ = Base.hx_preserve_ "true"

hx_prompt_ :: Text -> Attribute
hx_prompt_ = Base.hx_prompt_

hx_push_url_ :: Link -> Attribute
hx_push_url_ = Base.hx_delete_ . Servant.toUrlPiece

hx_put_ :: Link -> Attribute
hx_put_ = Base.hx_delete_ . Servant.toUrlPiece

data MaybeJavaScript a = JustValue a | Javascript Text

data HXRequestArg = HXRequestArg
    { hxRequestArgTimeout :: MaybeJavaScript Int
    , hxRequestArgCredentials :: MaybeJavaScript Bool
    , hxRequestArgNoHeaders :: MaybeJavaScript Bool
    }
    deriving (Eq, ToJSON)

hx_request_ :: HXRequestArg -> Attribute
hx_request_ = undefined

hx_select_ :: ToCssSelector a => a -> Attribute
hx_select_ = Base.hx_select_ . CssSelector.toCssSelector

-- More research
data HXSSEArg = HXSSEArg
    { hxSSEArgConnect :: Link
    , hxSSEArgSwap :: Text
    }
    deriving (Eq, Show, ToJSON)

hx_sse_ :: HXSSEArg -> Attribute
hx_sse_ = undefined

data HXSwapOOBArg = HXSwapOOBArgTrue | HXSwapOOBSwapArg HXSwapArg | HXSwapOOBSwapArgWithQuery HXSwapArg a -- Make it GADTs
    deriving (Eq, Show)

hx_swap_oob_ :: HXSwapOOB -> Attribute
hx_swap_oob_ = undefined

data SwapPos =
    SwapPosInner
    | SwapPosOuter
    | SwapPosBeforeBegin
    | SwapPosAfterBegin
    | SwapPosBeforeEnd
    | SwapPosAfterEnd
    | SwapPosNone
    deriving (Eq, Show)

data SwapModSwap where
    SwapModSwap :: Int -> SwapModSwap

data SwapModSettle where
    SwapModSettle :: Int -> SwapModSettle

data SwapModView where
    SwapModViewScroll :: Maybe ScrollSelector -> Maybe ScrollMove -> SwapModView
    SwapModViewShow :: Maybe ScrollSelector -> Maybe ScrollMove -> SwapModView

data ScrollSelector where
    ScrollSelectorQuery :: ToCssSelector a => a -> ScrollSelector
    ScrollSelectorWindow :: ScrollSelector

data ScrollMove = ScrollMoveTop | ScrollMoveBottom
    deriving (Eq, Show)

data HXSwapArg =
    { hxSwapArgPos :: SwapPos
    , hxSwapArgSwap :: Maybe SwapModSwap
    , hxSwapArgSettle :: Maybe SwapModSettle
    , hxSwapArgView :: SwapModView
    }
    deriving (Eq, Show)

hx_swap_ :: HXSwapArg -> Attribute
hx_swap_ = undefined

-- More research

