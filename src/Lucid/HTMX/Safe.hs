{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lucid.HTMX.Safe where

import qualified Css3.Selector as CssSelector
import Css3.Selector (ToCssSelector(..))
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.=))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
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

-- TODO: Get rid of Show instance and implement seperate function for displaying
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

data HXExtVal = HXExtVal (Set HTMXExt) | HXExtValIgnore (Set HTMXExt)
    deriving (Eq, Show)

hx_ext_ :: HXExtVal -> Attribute
hx_ext_ val = case val of
    HXExtVal htmxExtSet -> Base.hx_ext_ . Text.intercalate "," . Prelude.map (Text.pack . show) . Set.toList $ htmxExtSet
    HXExtValIgnore htmxExtSet -> Base.hx_ext_ . ("ignore:" <>) . Text.intercalate "," . Prelude.map (Text.pack . show) . Set.toList $ htmxExtSet

hx_get_ :: Link -> Attribute
hx_get_ = Base.hx_get_ . Servant.toUrlPiece

-- | Value of hx_headers_ must be valid JSON
hx_headers_ :: ToJSON a => a -> Attribute
hx_headers_ = Base.hx_headers_ . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

hx_history_elt_ :: Attribute
hx_history_elt_ = Base.hx_history_elt_

hx_include_ :: ToCssSelector a => a -> Attribute
hx_include_ = Base.hx_include_ . toCssSelector

data HXIndicatorVal where
    HXIndicatorVal :: ToCssSelector a => a -> HXIndicatorVal
    HXIndicatorValClosest :: ToCssSelector a => a -> HXIndicatorVal

hx_indicator_ :: HXIndicatorVal -> Attribute 
hx_indicator_ val = case val of
    HXIndicatorVal selector -> Base.hx_indicator_ . toCssSelector $ selector
    HXIndicatorValClosest selector -> Base.hx_indicator_ . ("closest " <>) . toCssSelector $ selector

data HXParamsVal where
    HXParamsVal :: [Text] -> HXParamsVal
    HXParamsValNot :: [Text] -> HXParamsVal
    HXParamsValAll :: HXParamsVal
    HXParamsValNone :: HXParamsVal
    deriving (Eq, Show)

hx_params_ :: HXParamsVal -> Attribute
hx_params_ val = case val of
    HXParamsVal params -> Base.hx_params_ . Text.intercalate "," $ params
    HXParamsValNot params -> Base.hx_params_ . ("not " <>) . Text.intercalate "," $ params
    HXParamsValAll -> Base.hx_params_ "*"
    HXParamsValNone -> Base.hx_params_ "none"

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
hx_push_url_ = Base.hx_push_url_ . Servant.toUrlPiece

hx_put_ :: Link -> Attribute
hx_put_ = Base.hx_put_ . Servant.toUrlPiece

data MaybeJavaScript a = JustValue a | JavaScript Text
    deriving (Eq, Show)

instance ToJSON a => ToJSON (MaybeJavaScript a) where
    toJSON :: MaybeJavaScript a -> Value
    toJSON mbJS = case mbJS of
        JustValue val -> toJSON val
        JavaScript expr -> String expr

{-
instance Show a => Show (MaybeJavaScript a) where
    show :: MaybeJavaScript a -> String
    show mbJS = case mbJS of
        JustValue val -> show val
        JavaScript expr -> Text.unpack expr
-}

data HXRequestVal = HXRequestVal
    { hxRequestValTimeout :: MaybeJavaScript Int
    , hxRequestValCredentials :: MaybeJavaScript Bool
    , hxRequestValNoHeaders :: MaybeJavaScript Bool
    }
    deriving (Eq, Show)

instance ToJSON HXRequestVal where
    toJSON :: HXRequestVal -> Value
    toJSON HXRequestVal{..} = Aeson.object
        [ "timeout" .= hxRequestValTimeout
        , "credentials" .= hxRequestValCredentials
        , "noHeaders" .= hxRequestValNoHeaders
        ]

hx_request_ :: HXRequestVal -> Attribute
hx_request_ val = Base.hx_request_ $ case val of
    (HXRequestVal (JavaScript _) _ _) -> ("javascript:" <>) . Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val
    (HXRequestVal _ (JavaScript _) _) -> ("javascript:" <>) . Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val
    (HXRequestVal _ _ (JavaScript _)) -> ("javascript:" <>) . Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val
    _ -> Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val

hx_select_ :: ToCssSelector a => a -> Attribute
hx_select_ = Base.hx_select_ . toCssSelector

data HXSSEVal = HXSSEVal
    { hxSSEValConnect :: Maybe Link
    , hxSSEValSwap :: Maybe Text
    }
    deriving (Show)

hx_sse_ :: HXSSEVal -> Attribute
hx_sse_ val = Base.hx_sse_ $ case val of
    (HXSSEVal Nothing Nothing) -> ""
    (HXSSEVal (Just link) Nothing) -> "connect:" <> (Servant.toUrlPiece link)
    (HXSSEVal Nothing (Just eventName)) -> "swap:" <> eventName
    (HXSSEVal (Just link) (Just eventName)) -> "connect:" <> (Servant.toUrlPiece link) <> " " <> "swap:" <> eventName

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
    deriving (Eq, Show)

data SwapModSettle where
    SwapModSettle :: Int -> SwapModSettle
    deriving(Eq, Show)

data ScrollSelector where
    ScrollSelector :: forall a. (Eq a, Show a, ToCssSelector a) => a -> ScrollSelector
    ScrollSelectorWindow :: ScrollSelector

instance Show ScrollSelector where
    show :: ScrollSelector -> String
    show ss = case ss of
        ScrollSelector q -> "ScrollSelector " <> show q
        ScrollSelectorWindow -> "ScrollSelectorWindow"

instance Eq ScrollSelector where
    ss1 == ss2 = show ss1 == show ss2

data ScrollMove = ScrollMoveTop | ScrollMoveBottom
    deriving (Eq, Show)

data SwapModView where
    SwapModViewScroll :: Maybe ScrollSelector -> Maybe ScrollMove -> SwapModView
    SwapModViewShow :: Maybe ScrollSelector -> Maybe ScrollMove -> SwapModView
    deriving (Eq, Show)

data HXSwapVal = HXSwapVal
    { hxSwapValPos :: SwapPos
    , hxSwapValSwap :: Maybe SwapModSwap
    , hxSwapValSettle :: Maybe SwapModSettle
    , hxSwapValView :: SwapModView
    }
    deriving (Eq, Show)

hx_swap_ :: HXSwapVal -> Attribute
hx_swap_ = undefined

data HXSwapOOBVal where
    HXSwapOOBVal :: HXSwapOOBVal
    HXSwapOOBValSwap :: HXSwapVal -> HXSwapOOBVal
    HXSwapOOBValSwapSelector :: forall a. (Eq a, Show a, ToCssSelector a) => HXSwapVal -> a -> HXSwapOOBVal

instance Show HXSwapOOBVal where
    show :: HXSwapOOBVal -> String
    show HXSwapOOBVal = "HXSwapOOBVal"
    show (HXSwapOOBValSwap hxSwapVal) = "HXSwapOOBValSwap " <> show hxSwapVal
    show (HXSwapOOBValSwapSelector hxSwapVal sel) = "HXSwapOOBValSwap " <> show hxSwapVal <> " " <> show sel

instance Eq HXSwapOOBVal where
    val1 == val2 = show val1 == show val2

hx_swap_oob_ :: HXSwapOOBVal -> Attribute
hx_swap_oob_ = undefined

data HXTargetVal where
    HXTargetVal :: HXTargetVal --TODO: Keep like normal or add This suffix?
    HXTargetValSelector :: (Eq a, Show a, ToCssSelector a) => a -> HXTargetVal
    HXTargetValSelectorClosest :: (Eq a, Show a, ToCssSelector a) => a -> HXTargetVal
    HXTargetValSelectorFind :: (Eq a, Show a, ToCssSelector a) => a -> HXTargetVal

hx_target_ :: HXTargetVal -> Attribute
hx_target_ = undefined

-- TODO: Study more. Basically all possible events plus event modifiers.
type HXTriggerVal = Text

hx_trigger_ :: HXTriggerVal -> Attribute
hx_trigger_ = Base.hx_trigger_

hx_vals_ :: ToJSON a => a -> Attribute
hx_vals_ = Base.hx_vals_ . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

-- BELOW EXPERIMENTAL!!

type HXWSVal = Text

hx_ws_ :: HXWSVal -> Attribute
hx_ws_ = Base.hx_ws_

-- TODO: Add QuasiQuoters for parsing and generating values that are checked at compile time for the various arguments to the HTMX attributes.
-- TODO: Write tests to check that the Val types are generating the correct Text for the HTMX attributes. Tests for HTMX tag functionality maybe?

