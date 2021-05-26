{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where


--todo: explain all of the imports
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function
import Data.Functor.Identity
import Database.Id.Class (Id)
import Database.Id.Obelisk.Route (idPathSegmentEncoder)
import Obelisk.Route
import Obelisk.Route.TH
import Common.Schema

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute () -- Used to handle unparseable routes.
  BackendRoute_NewTh :: BackendRoute () -- to make a new thread
  BackendRoute_NewCom :: BackendRoute () -- to make a new comment
  BackendRoute_ListPosts :: BackendRoute () -- to get recent posts
  BackendRoute_GetPost :: BackendRoute (Id Post) -- to get a single thread

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute () -- home page
  FrontendRoute_ViewPost :: FrontendRoute (Id Post) -- thread page

checkedFullRouteEncoder
  :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = checkEncoder fullRouteEncoder & \case
  Left err -> error $ T.unpack err
  Right encoder -> encoder

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_NewTh -> PathSegment "thread" $ unitEncoder mempty
      BackendRoute_NewCom -> PathSegment "comment" $ unitEncoder mempty
      BackendRoute_ListPosts -> PathSegment "list" $ unitEncoder mempty
      BackendRoute_GetPost -> PathSegment "post" idPathSegmentEncoder
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_ViewPost -> PathSegment "p" idPathSegmentEncoder)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
