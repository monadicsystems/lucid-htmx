module Lucid.Htmx where

import Lucid.Base
import Data.Text (Text)

-- | <https://htmx.org/attributes/hx-boost/>
hxBoost_ :: Text -> Attributes
hxBoost_ = makeAttributes "data-hx-boost"

-- | <https://htmx.org/attributes/hx-confirm/>
hxConfirm_ :: Text -> Attributes
hxConfirm_ = makeAttributes "data-hx-confirm"

-- | <https://htmx.org/attributes/hx-delete/>
hxDelete_ :: Text -> Attributes
hxDelete_ = makeAttributes "data-hx-delete"

-- | <https://htmx.org/attributes/hx-disable/>
hxDisable_ :: Attributes
hxDisable_ = makeAttributes "data-hx-disable" mempty

-- | <https://htmx.org/attributes/hx-encoding/>
hxEncoding_ :: Text -> Attributes
hxEncoding_ = makeAttributes "data-hx-encoding"

-- | <https://htmx.org/attributes/hx-ext/>
hxExt_ :: Text -> Attributes
hxExt_ = makeAttributes "data-hx-ext"

-- | <https://htmx.org/attributes/hx-get/>
hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "data-hx-get"

-- | <https://htmx.org/attributes/hx-headers/>
hxHeaders_ :: Text -> Attributes
hxHeaders_ = makeAttributes "data-hx-headers"

-- | <https://htmx.org/attributes/hx-history-elt/>
hxHistoryElt_ :: Attributes
hxHistoryElt_ = makeAttributes "data-hx-history-elt" mempty

-- | <https://htmx.org/attributes/hx-include/>
hxInclude_ :: Text -> Attributes
hxInclude_ = makeAttributes "data-hx-include"

-- | <https://htmx.org/attributes/hx-indicator/>
hxIndicator_ :: Text -> Attributes
hxIndicator_ = makeAttributes "data-hx-indicator"

-- | <https://htmx.org/attributes/hx-params/>
hxParams_ :: Text -> Attributes
hxParams_ = makeAttributes "data-hx-params"

-- | <https://htmx.org/attributes/hx-patch/>
hxPatch_ :: Text -> Attributes
hxPatch_ = makeAttributes "data-hx-patch"

-- | <https://htmx.org/attributes/hx-post/>
hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "data-hx-post"

-- | <https://htmx.org/attributes/hx-preserve/>
hxPreserve_ :: Text -> Attributes
hxPreserve_ = makeAttributes "data-hx-preserve"

-- | <https://htmx.org/attributes/hx-prompt/>
hxPrompt_ :: Text -> Attributes
hxPrompt_ = makeAttributes "data-hx-prompt"

-- | <https://htmx.org/attributes/hx-push-url/>
hxPushUrl_ :: Text -> Attributes
hxPushUrl_ = makeAttributes "data-hx-push-url"

-- | <https://htmx.org/attributes/hx-put/>
hxPut_ :: Text -> Attributes
hxPut_ = makeAttributes "data-hx-put"

-- | <https://htmx.org/attributes/hx-request/>
hxRequest_ :: Text -> Attributes
hxRequest_ = makeAttributes "data-hx-request"

-- | <https://htmx.org/attributes/hx-select/>
hxSelect_ :: Text -> Attributes
hxSelect_ = makeAttributes "data-hx-select"

-- | <https://htmx.org/attributes/hx-sse/>
hxSse_ :: Text -> Attributes
hxSse_ = makeAttributes "data-hx-sse"

-- | <https://htmx.org/attributes/hx-swap-oob/>
hxSwapOob_ :: Text -> Attributes
hxSwapOob_ = makeAttributes "data-hx-swap-oob"

-- | <https://htmx.org/attributes/hx-swap/>
hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "data-hx-swap"

-- | <https://htmx.org/attributes/hx-target/>
hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "data-hx-target"

-- | <https://htmx.org/attributes/hx-trigger/>
hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "data-hx-trigger"

-- | <https://htmx.org/attributes/hx-vals/>
hxVals_ :: Text -> Attributes
hxVals_ = makeAttributes "data-hx-vals"

-- | <https://htmx.org/attributes/hx-ws/>
hxWs_ :: Text -> Attributes
hxWs_ = makeAttributes "data-hx-ws"
