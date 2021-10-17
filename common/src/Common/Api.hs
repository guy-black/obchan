{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import Data.Text as T
import qualified Data.Map as M
import GHC.Generics (Generic)
import GHC.Int
import Common.Schema (Post(..))
import Database.Id.Class (Id(..), HasId)
import Data.Time as Time
import Reflex.Dom.Xhr.FormData
import Reflex.Dom.Core
import JSDOM.Types


{-
data PostRequest = PostRequest {
                 _postRequest_image :: Bool
                ,_postRequest_content :: Text
                ,_postRequest_op :: Maybe (Id PostResponse)
                }
  deriving stock Generic deriving (ToJSON, FromJSON)

-}



data PostFetch = PostFetch {
                  _page :: Int
                 ,_amount :: Int
                 }
  deriving stock Generic deriving (ToJSON, FromJSON)                 

data PostResponse = PostResponse {
                  _postResponse_id :: Int64
                 ,_postResponse_image :: Bool
                 ,_postResponse_content :: T.Text
                 ,_postResponse_datetime :: Time.LocalTime
                 }
  deriving stock Generic deriving (ToJSON, FromJSON)

instance HasId PostResponse

data ThreadResponse = ThreadResponse {
                    _threadResponse_op :: PostResponse
                   ,_threadResponse_replies :: [PostResponse]
                   ,_threadResponse_comCount :: Int
                   }
  deriving stock Generic deriving (ToJSON, FromJSON)
