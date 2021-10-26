module Lucid.HTMX.QuasiQuoters where

{- Idea for QuasiQuoting syntax:

[htmx| hx-get="bar/foo/baz?id=34" hx-swap="delay:500" hx-trigger="click" |]

-}