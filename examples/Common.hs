{-# LANGUAGE OverloadedStrings #-}

module Common where

import Data.Text
import Lucid

blankHtml :: Html ()
blankHtml = ""

baseHtml :: Monad m => Text -> HtmlT m a -> HtmlT m a
baseHtml title innerHtml = do
    doctype_

    html_ [lang_ "en"] ""

    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

        title_ $ toHtml title

        link_ [href_ "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css", rel_ "stylesheet"]
        script_ [src_ "https://unpkg.com/htmx.org@1.5.0"] blankHtml

    body_ innerHtml


textStyle_ = classes_ ["text-xl", "text-semibold"]

buttonStyle_ color = classes_ ["px-4", "py-2", "bg-"<>color, "text-lg", "text-white", "rounded-md", "mt-5"]
