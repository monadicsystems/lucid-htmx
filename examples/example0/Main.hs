{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Css3.Selector (csssel)
import qualified Css3.Selector as Css3
import qualified Data.Aeson as Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import GHC.TypeLits
import Lucid
import Lucid.HTMX.Safe
import Network.Wai.Handler.Warp
import Servant.API
import Servant.HTML.Lucid
import Servant.Links
import Servant.Server


data HomepageData = HomepageData
    { homepageDataLine1 :: Text
    , homepageDataLine2 :: Text
    }
    deriving (Eq, Show)

data MyData = MyData
    { myDataName :: Text
    , myDataFavColor :: Text
    }
    deriving (Eq, Show)

-- Homepage path, proxy, and handler
type Homepage = Get '[HTML] HomepageData

-- Clicked path, proxy, and handler
type Clicked = "clicked" :> Get '[HTML] MyData

-- Whole API (could possibly have more endpoints)
type API = Homepage :<|> Clicked

homepageHandler :: Handler HomepageData
homepageHandler = return $ HomepageData
    { homepageDataLine1 = "Welcome to my cool website that I built with Haskell, HTMX, and TailwindCSS!"
    , homepageDataLine2 = "Click the button so I can introduce myself."
    }

clickedHandler :: Handler MyData
clickedHandler = return $ MyData
    { myDataName = "Rashad"
    , myDataFavColor = "green"
    }

-- Server
server :: Server API
server = homepageHandler :<|> clickedHandler

homepageEndpoint :: Proxy Homepage
homepageEndpoint = Proxy

clickedEndpoint :: Proxy Clicked
clickedEndpoint = Proxy

api :: Proxy API
api = Proxy

homepageDataLink :: Link
homepageDataLink = safeLink api homepageEndpoint

clickedLink :: Link
clickedLink = safeLink api clickedEndpoint

instance ToHtml HomepageData where
    toHtml HomepageData{..} = baseHtml "Example 0" $ do
    -- TODO: Make QuasiQuoter for lucid attributes
        div_ [id_ "parent-div", class_ "m-20"] $ do
            p_ [textStyle_] $ toHtml homepageDataLine1
            p_ [textStyle_] $ toHtml homepageDataLine2
            button_
                [ hx_get_ clickedLink
                , hx_swap_ (HXSwapVal SwapPosInner Nothing Nothing Nothing)
                , hx_target_ (HXTargetValSelector [csssel|#parent-div|])
                , buttonStyle_ "green-400"
                ]
                "Click Me"
    toHtmlRaw = toHtml

instance ToHtml MyData where
    toHtml MyData{..} = do
        p_ [textStyle_] $ "Hello, my name is " <> (toHtml myDataName) <> "."
        p_ [textStyle_] $ "My favorite color is " <> (span_ [class_ "text-green-500"] (toHtml myDataFavColor)) <> "."
        p_ [textStyle_] $ "Nice to meet you!"
        p_ [textStyle_] $ "Click the button below to go back!"
        button_ 
            [ hx_get_ homepageDataLink
            , hx_swap_ (HXSwapVal SwapPosOuter Nothing Nothing Nothing)
            , hx_target_ (HXTargetValSelector [csssel|#parent-div|])
            , buttonStyle_ "red-400"
            ]
            "Go Back"
    toHtmlRaw = toHtml

main :: IO ()
main = do
    let application = serve @API Proxy server
        port = 8080
    
    print $ "Serving application on port: " <> (show port)
    run port application
