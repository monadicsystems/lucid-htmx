{-# LANGUAGE OverloadedStrings #-}

module Lucid.HTMX.Base.Attributes where

import Lucid.Base (Attribute, makeAttribute)
import Data.Text (Text)

-- | <https://htmx.org/attributes/hx-boost/>
hx_boost_ :: Text -> Attribute
hx_boost_ = makeAttribute "hx-boost"

-- | <https://htmx.org/attributes/hx-confirm/>
hx_confirm_ :: Text -> Attribute
hx_confirm_ = makeAttribute "hx-confirm"

-- | <https://htmx.org/attributes/hx-delete/>
hx_delete_ :: Text -> Attribute
hx_delete_ = makeAttribute "hx-delete"

-- | <https://htmx.org/attributes/hx-disable/>
hx_disable_ :: Attribute
hx_disable_ = makeAttribute "hx-disable" mempty

-- | <https://htmx.org/attributes/hx-encoding/>
hx_encoding_ :: Text -> Attribute
hx_encoding_ = makeAttribute "hx-encoding"

-- | <https://htmx.org/attributes/hx-ext/>
hx_ext_ :: Text -> Attribute
hx_ext_ = makeAttribute "hx-ext"

-- | <https://htmx.org/attributes/hx-get/>
hx_get_ :: Text -> Attribute
hx_get_ = makeAttribute "hx-get"

-- | <https://htmx.org/attributes/hx-headers/>
hx_headers_ :: Text -> Attribute
hx_headers_ = makeAttribute "hx-headers"

-- | <https://htmx.org/attributes/hx-history-alt/>
hx_history_elt_ :: Attribute
hx_history_elt_ = makeAttribute "hx-history-elt" mempty

-- | <https://htmx.org/attributes/hx-include/>
hx_include_ :: Text -> Attribute
hx_include_ = makeAttribute "hx_include"

-- | <https://htmx.org/attributes/hx-indicator/>
hx_indicator_ :: Text -> Attribute
hx_indicator_ = makeAttribute "hx-indicator"

-- | <https://htmx.org/attributes/hx-params/>
hx_params_ :: Text -> Attribute
hx_params_ = makeAttribute "hx-params"

-- | <https://htmx.org/attributes/hx-patch/>
hx_patch_ :: Text -> Attribute
hx_patch_ = makeAttribute "hx-patch"

-- | <https://htmx.org/attributes/hx-post/>
hx_post_ :: Text -> Attribute
hx_post_ = makeAttribute "hx-post"

-- | <https://htmx.org/attributes/hx-preserve/>
hx_preserve_ :: Text -> Attribute
hx_preserve_ = makeAttribute "hx-preserve"

-- | <https://htmx.org/attributes/hx-prompt/>
hx_prompt_ :: Text -> Attribute
hx_prompt_ = makeAttribute "hx-prompt"

-- | <https://htmx.org/attributes/hx-push-url/>
hx_push_url_ :: Text -> Attribute
hx_push_url_ = makeAttribute "hx-push-url"

-- | <https://htmx.org/attributes/hx-put/>
hx_put_ :: Text -> Attribute
hx_put_ = makeAttribute "hx-put"

-- | <https://htmx.org/attributes/hx-request/>
hx_request_ :: Text -> Attribute
hx_request_ = makeAttribute "hx-request"

-- | <https://htmx.org/attributes/hx-select/>
hx_select_ :: Text -> Attribute
hx_select_ = makeAttribute "hx-select"

-- | <https://htmx.org/attributes/hx-sse/>
hx_sse_ :: Text -> Attribute
hx_sse_ = makeAttribute "hx-sse"

-- | <https://htmx.org/attributes/hx-swap-oob/>
hx_swap_oob_ :: Text -> Attribute
hx_swap_oob_ = makeAttribute "hx-swap-oob"

-- | <https://htmx.org/attributes/hx-swap/>
hx_swap_ :: Text -> Attribute
hx_swap_ = makeAttribute "hx-swap"

-- | <https://htmx.org/attributes/hx-target/>
hx_target_ :: Text -> Attribute
hx_target_ = makeAttribute "hx-target"

-- | <https://htmx.org/attributes/hx-trigger/>
hx_trigger_ :: Text -> Attribute
hx_trigger_ = makeAttribute "hx-trigger"

-- | <https://htmx.org/attributes/hx-vals/>
hx_vals_ :: Text -> Attribute
hx_vals_ = makeAttribute "hx-vals"

-- | <https://htmx.org/attributes/hx-ws/>
hx_ws_ :: Text -> Attribute
hx_ws_ = makeAttribute "hx-ws"
