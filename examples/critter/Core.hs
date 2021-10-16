{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Core where

import Common
import Contravariant.Extras.Contrazip (contrazip3)
import Control.Monad.IO.Class (liftIO)
import Css3.Selector (csssel)
import Data.Functor.Identity (Identity)
import Data.Int
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Proxy
import Data.Time
import Data.Text
import Data.Tuple.Curry
import Data.Vector (Vector)
import GHC.Generics
import GHC.TypeLits
import Hasql.TH
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import Lucid
import Lucid.HTMX (hx_include_, hx_target_)
import Lucid.HTMX.Safe hiding (hx_include_, hx_target_)
import Network.Wai.Handler.Warp
import Prelude
import Servant.API
import Servant.HTML.Lucid
import Servant.Links
import Servant.Server

import qualified Css3.Selector as Css3
import qualified Data.Aeson as Aeson
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection


newtype ID a = ID { unID :: Int32 }
    deriving stock (Eq)
    deriving newtype (Show)

newtype Secret = Secret { unSecret :: Text }
    deriving stock (Eq)
    deriving newtype (Show)

data UserAuth = UserAuth
    { userAuthSecret1 :: Secret
    , userAuthSecret2 :: Secret
    }
    deriving stock (Eq, Show)

newtype Tag = Tag { unTag :: Text }
    deriving stock (Eq)
    deriving newtype (Show)

data Animal =
    Elephant
    -- An elephant whose account is older than 6 months is immune to downvotes, except by lions
    | Lion
    -- A lion can downvote/upvote a post/tweet up to two times, and downvote elephants post's/tweets once
    | Zebra
    -- A zebra has the ability to delete their posts/tweets
    | Chameleon
    -- A chameleon can edit their posts/tweets
    | Parrot
    -- A parrot can post/comment as any other animal, but get's none of their benefits
    | Eagle
    -- Can see a zebra's deleted comments, a chameleon's edits, and a parrot through disguise
    | Scorpion
    -- If a scorpion replies to a comment and gets more votes than the post they reply to,
    -- the original comment/post poster is banned for 24 hours. Doesn't work on other scorpions
    deriving stock (Eq, Show)

data NewUser = NewUser
    { newUserSecret1 :: Secret
    , newUserSecret1Confirm :: Secret
    , newUserSecret2 :: Secret
    , newUserSecret2Confirm :: Secret
    , newUserIntro :: Text
    , newUserTags :: [Tag]
    , newUserAnimal :: Animal
    }
    deriving stock (Eq, Show)

data AuthorizedUser = AuthorizedUser
    { authorizedUserID :: ID AuthorizedUser
    , authorizedUserIntro :: Text
    , authorizedUserTags :: [Tag]
    , authorizedUserAnimal :: Animal
    }
    deriving stock (Eq, Show)

data NewCreet = NewCreet
    { newCreetUserID :: ID AuthorizedUser
    , newCreetParentID :: Maybe (ID Creet)
    , newCreetTimestamp :: UTCTime
    , newCreetContent :: Text
    , newCreetTags :: [Tag]
    }
    deriving stock (Eq, Show)

-- | Analogous to a tweet
data Creet = Creet
    { creetID :: ID Creet
    , creetUserID :: ID AuthorizedUser
    -- , creetParentID :: Maybe (ID Creet)
    , creetTimestamp :: UTCTime
    , creetContent :: Text
    , creetPositive :: Int
    , creetNegative :: Int
    -- , creetChildren [ID Creet]
    , creetTags :: [Tag]
    }
    deriving stock (Eq, Show)
