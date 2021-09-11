{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.HTMX.Safe.Types where

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

type HXParamArg = [Text]

data MaybeJavaScript a = JustValue a | JavaScript Text
    deriving (Eq)

instance Show (MaybeJavaScript a) where
    show :: MaybeJavaScript a -> String
    show mbJS = case mbJS of
        JustValue val -> show val
        JavaScript expr -> "javascript:" <> (Text.unpack expr)

data HXRequestArg = HXRequestArg
    { hxRequestArgTimeout :: MaybeJavaScript Int
    , hxRequestArgCredentials :: MaybeJavaScript Bool
    , hxRequestArgNoHeaders :: MaybeJavaScript Bool
    }
    deriving (Eq, ToJSON)

data HXSSEArg = HXSSEArg
    { hxSSEArgConnect :: Link
    , hxSSEArgSwap :: Text
    }
    deriving (Show, ToJSON)

data HXSwapOOBArg where
    HXSwapOOBArgTrue :: HXSwapOOBArg
    HXSwapOOBArgSwap :: HXSwapArg -> HXSwapOOBArg
    HXSwapOOBArgSwapWithQuery :: ToCssSelector a => HXSwapArg -> a -> HXSwapOOBArg
    deriving (Eq, Show)

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

data SwapModView where
    SwapModViewScroll :: Maybe ScrollSelector -> Maybe ScrollMove -> SwapModView
    SwapModViewShow :: Maybe ScrollSelector -> Maybe ScrollMove -> SwapModView
    deriving (Eq, Show)

data ScrollSelector where
    ScrollSelectorQuery :: ToCssSelector a => a -> ScrollSelector
    ScrollSelectorWindow :: ScrollSelector
    deriving (Eq, Show)

data ScrollMove = ScrollMoveTop | ScrollMoveBottom
    deriving (Eq, Show)

data HXSwapArg = HXSwapArg
    { hxSwapArgPos :: SwapPos
    , hxSwapArgSwap :: Maybe SwapModSwap
    , hxSwapArgSettle :: Maybe SwapModSettle
    , hxSwapArgView :: SwapModView
    }
    deriving (Eq, Show)

