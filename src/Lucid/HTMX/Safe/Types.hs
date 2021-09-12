{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lucid.HTMX.Safe.Types where

import Css3.Selector (ToCssSelector(..))
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON(..), Value(..), (.=))
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import Servant.Links (Link)

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
    | OtherHTMXExt Text
    deriving (Eq, Show)

data HXExtVal = HXExtVal (Set HTMXExt) | HXExtValIgnore (Set HTMXExt)
    deriving (Eq, Show)

data HXIndicatorVal where
    HXIndicatorVal :: ToCssSelector a => a -> HXIndicatorVal
    HXIndicatorValClosest :: ToCssSelector a => a -> HXIndicatorVal

data HXParamsVal where
    HXParamsVal :: [Text] -> HXParamsVal
    HXParamsValNot :: [Text] -> HXParamsVal
    HXParamsValAll :: HXParamsVal
    HXParamsValNone :: HXParamsVal
    deriving (Eq, Show)

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

data HXSSEVal = HXSSEVal
    { hxSSEValConnect :: Maybe Link
    , hxSSEValSwap :: Maybe Text
    }
    deriving (Show)

-- TODO: Come up with better, shorter, more intuitive names for types
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
        ScrollSelector q -> "ScrollSelector " <> (Text.unpack $ toCssSelector q)
        ScrollSelectorWindow -> "ScrollSelectorWindow"

instance Eq ScrollSelector where
    ss1 == ss2 = show ss1 == show ss2

data ScrollMove = ScrollMoveTop | ScrollMoveBottom
    deriving (Eq, Show)

data SwapModViewType = SwapModViewTypeScroll | SwapModViewTypeShow
    deriving (Eq, Show)

data SwapModView where
    SwapModView :: SwapModViewType -> ScrollMove -> Maybe ScrollSelector -> SwapModView
    deriving (Eq, Show)

data HXSwapVal = HXSwapVal
    { hxSwapValPos :: SwapPos
    , hxSwapValSwap :: Maybe SwapModSwap -- Call this delay??
    , hxSwapValSettle :: Maybe SwapModSettle
    , hxSwapValView :: Maybe SwapModView
    }
    deriving (Eq, Show)

data HXSwapOOBVal where
    HXSwapOOBVal :: HXSwapOOBVal
    HXSwapOOBValSwap :: SwapPos -> HXSwapOOBVal
    HXSwapOOBValSwapSelector :: forall a. (Eq a, Show a, ToCssSelector a) => SwapPos -> a -> HXSwapOOBVal

instance Show HXSwapOOBVal where
    show :: HXSwapOOBVal -> String
    show HXSwapOOBVal = "HXSwapOOBVal"
    show (HXSwapOOBValSwap hxSwapVal) = "HXSwapOOBValSwap " <> show hxSwapVal
    show (HXSwapOOBValSwapSelector hxSwapVal sel) = "HXSwapOOBValSwap " <> show hxSwapVal <> " " <> show sel

instance Eq HXSwapOOBVal where
    val1 == val2 = show val1 == show val2

data HXTargetVal where
    HXTargetVal :: HXTargetVal --TODO: Keep like normal or add "This" suffix?
    HXTargetValSelector :: (Eq a, Show a, ToCssSelector a) => a -> HXTargetVal
    HXTargetValSelectorClosest :: (Eq a, Show a, ToCssSelector a) => a -> HXTargetVal
    HXTargetValSelectorFind :: (Eq a, Show a, ToCssSelector a) => a -> HXTargetVal

-- TODO: Study more. Basically all possible events plus event modifiers.
-- type HXTriggerVal = Text

type HXTriggerVal = Text

-- BELOW EXPERIMENTAL!!

type HXWSVal = Text

-- TODO: Add QuasiQuoters for parsing and generating values that are checked at compile time for the various arguments to the HTMX attributes.
-- TODO: Write tests to check that the Val types are generating the correct Text for the HTMX attributes. Tests for HTMX tag functionality maybe?
