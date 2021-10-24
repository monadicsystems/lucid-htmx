{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lucid.HTMX.Safe where

import HTMX.Types
import Lucid.HTMX
import System.Console.Terminfo (Attributes)

toUrlPiece' = ("/" <>) . toUrlPiece

-- -- | Makes hx_boost_ a "boolean attribute" since the only valid value for hx-boost is "true".
-- hx_boost_ :: Attribute
-- hx_boost_ = Base.hx_boost_ "true"

-- hx_confirm_ :: Text -> Attribute
-- hx_confirm_ = Base.hx_confirm_

hxDeleteSafe_ :: Link -> Attribute
hxDeleteSafe_ = Base.hx_delete_ . toUrlPiece'

-- hx_disable_ :: Attribute
-- hx_disable_ = Base.hx_disable_

-- | Makes hx_encoding_ a "boolean attribute" since the only valid value for hx-encoding is "multipart/form-data".
-- hx_encoding_ :: Attribute
-- hx_encoding_ = Base.hx_encoding_ "multipart/form-data"

getHTMXExtName :: HTMXExtension -> Text
getHTMXExtName htmxExt = case htmxExt of
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
    OtherHTMXExtension extName -> extName

hxExtSafe_ :: HXExt -> Attribute
hxExtSafe_ HXExt{..} =
   hxExt_
   . ((if hxExtIgnore then "ignore:" else "") <>)
   . Text.intercalate ","
   . Prelude.map getHTMXExtName
   . HashSet.toList
   $ hxExtExtensions
   -- HXExtVal htmxExtHashSet -> Base.hx_ext_ . Text.intercalate "," . Prelude.map getHTMXExtName . HashSet.toList $ htmxExtHashSet
   -- HXExtValIgnore htmxExtHashSet -> Base.hx_ext_ . ("ignore:" <>) . Text.intercalate "," . Prelude.map getHTMXExtName . HashSet.toList $ htmxExtHashSet

hxGetSafe_ :: Link -> Attribute
hxGetSafe_ = Base.hx_get_ . toUrlPiece'

-- | Value of hx_headers_ must be valid JSON
hxHeadersSafe_ :: HXHeaders -> Attribute
hxHeadersSafe_ HXHeaders{..} =
   Base.hx_headers_
   . Text.decodeUtf8
   . LBS.toStrict
   . Aeson.encode
   $ hxHeadersJSON

-- hx_history_elt_ :: Attribute
-- hx_history_elt_ = Base.hx_history_elt_

hxIncludeSafe_ :: HXInclude -> Attribute
hxIncludeSafe_ HXInclude{..} = Base.hx_include_ . toCssSelector

hxIndicatorSafe_ :: HXIndicator -> Attribute
hxIndicatorSafe_ HXIndicator{..} =
    hxIndicator_
    . ((if hxIndicatorClosest then "closest:" else "") <>)
    . toCssSelector
    $ hxIndicatorSelector

-- hx_indicator_ :: HXIndicatorVal -> Attribute 
-- hx_indicator_ val = case val of
--     HXIndicatorVal selector -> Base.hx_indicator_ . toCssSelector $ selector
--     HXIndicatorValClosest selector -> Base.hx_indicator_ . ("closest " <>) . toCssSelector $ selector

hxParamsSafe_ :: HXParams -> Attribute
hxParamsSafe_ hxParams = case hxParams of
    HXParams params -> hxParams_ . Text.intercalate "," $ params
    HXParamsNot params -> hxParams_ . ("not " <>) . Text.intercalate "," $ params
    HXParamsAll -> hxParams_ "*"
    HXParamsNone -> hxParams_ "none"

hx_patch_ :: Link -> Attribute
hx_patch_ = Base.hx_patch_ . toUrlPiece'

hx_post_ :: Link -> Attribute
hx_post_ = Base.hx_post_ . toUrlPiece'

-- For same reasons as hx_boost_
hx_preserve_ :: Attribute
hx_preserve_ = Base.hx_preserve_ "true"

hx_prompt_ :: Text -> Attribute
hx_prompt_ = Base.hx_prompt_

hx_push_url_ :: Link -> Attribute
hx_push_url_ = Base.hx_push_url_ . toUrlPiece'

hx_put_ :: Link -> Attribute
hx_put_ = Base.hx_put_ . toUrlPiece'

hx_request_ :: HXRequestVal -> Attribute
hx_request_ val = Base.hx_request_ $ case val of
    (HXRequestVal (JavaScript _) _ _) -> ("javascript:" <>) . Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val
    (HXRequestVal _ (JavaScript _) _) -> ("javascript:" <>) . Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val
    (HXRequestVal _ _ (JavaScript _)) -> ("javascript:" <>) . Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val
    _ -> Text.decodeUtf8 . LBS.toStrict . Aeson.encode $ val

hx_select_ :: ToCssSelector a => a -> Attribute
hx_select_ = Base.hx_select_ . toCssSelector

hx_sse_ :: HXSSEVal -> Attribute
hx_sse_ val = Base.hx_sse_ $ case val of
    (HXSSEVal Nothing Nothing) -> ""
    (HXSSEVal (Just link) Nothing) -> "connect:" <> (toUrlPiece' link)
    (HXSSEVal Nothing (Just eventName)) -> "swap:" <> eventName
    (HXSSEVal (Just link) (Just eventName)) -> "connect:" <> (toUrlPiece' link) <> " " <> "swap:" <> eventName

pos :: SwapPos -> Text
pos p = case p of
    SwapPosInner -> "innerHTML"
    SwapPosOuter -> "outerHTML"
    SwapPosBeforeBegin -> "beforebegin"
    SwapPosAfterBegin -> "afterbegin"
    SwapPosBeforeEnd -> "beforeend"
    SwapPosAfterEnd -> "afterend"
    SwapPosNone -> "none"

hx_swap_ :: HXSwapVal -> Attribute
hx_swap_ HXSwapVal{..} = Base.hx_swap_ $ (pos hxSwapValPos) <> (swap hxSwapValSwap) <> (settle hxSwapValSettle) <> (view hxSwapValView)
    where
        swap :: Maybe SwapModDelay -> Text
        swap s = case s of
            Nothing -> ""
            Just (SwapModDelay delay) -> " swap:" <> (Text.pack $ show delay) <> "s"

        settle :: Maybe SwapModSettle -> Text
        settle s = case s of
            Nothing -> ""
            Just (SwapModSettle delay) -> " settle:" <> (Text.pack $ show delay) <> "s"

        view :: Maybe SwapModView -> Text
        view v = case v of
            Nothing -> ""
            Just v' -> " " <> (viewPrefix v') <> (viewPostfix v')
            where
                viewPostfix :: SwapModView -> Text
                viewPostfix v' = case v' of
                    SwapModView _ sm ss -> case (sm, ss) of
                        (ScrollMoveTop, ss') -> (selectorPrefix ss') <> "top"
                        (ScrollMoveBottom, ss') -> (selectorPrefix ss') <> "bottom"

                selectorPrefix :: Maybe ScrollSelector -> Text
                selectorPrefix ss = case ss of
                    Nothing -> ""
                    Just ss' -> case ss' of
                        ScrollSelector q -> (toCssSelector q) <> ":"
                        ScrollSelectorWindow -> "window:"

                viewPrefix :: SwapModView -> Text
                viewPrefix v' = case v' of
                      SwapModView SwapModViewTypeScroll _ _ -> "scroll:"
                      SwapModView SwapModViewTypeShow _ _ -> "show:"

hx_swap_oob_ :: HXSwapOOBVal -> Attribute
hx_swap_oob_ val = Base.hx_swap_oob_ $ case val of
    HXSwapOOBVal -> "true"
    HXSwapOOBValSwap swapPos -> pos swapPos
    HXSwapOOBValSwapSelector swapPos selector -> (pos swapPos) <> ":" <> (toCssSelector selector)

hx_target_ :: HXTargetVal -> Attribute
hx_target_ val = Base.hx_target_ $ case val of
    HXTargetVal -> "this"
    HXTargetValSelector selector -> toCssSelector selector
    HXTargetValSelectorClosest selector -> "closest " <> (toCssSelector selector)
    HXTargetValSelectorFind selector -> "find " <> (toCssSelector selector) 

hx_vals_ :: ToJSON a => a -> Attribute
hx_vals_ = Base.hx_vals_ . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

-- TODO: Study more. Basically all possible events plus event modifiers.

hx_trigger_ :: HXTriggerVal -> Attribute
hx_trigger_ = Base.hx_trigger_

-- BELOW EXPERIMENTAL!!

hx_ws_ :: HXWSVal -> Attribute
hx_ws_ = Base.hx_ws_

-- TODO: Add QuasiQuoters for parsing and generating values that are checked at compile time for the various arguments to the HTMX attributes.
-- TODO: Write tests to check that the Val types are generating the correct Text for the HTMX attributes. Tests for HTMX tag functionality maybe?

