{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import GHC.Int
import Database.Id.Class (Id(..))
import qualified Data.Map as M
import Data.Text (Text)
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Api
import Common.Route
import Common.Schema

type AppWidget js t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PerformEvent t m
  , PostBuild t m
  , Prerender js t m
  , TriggerEvent t m
  )

type WidgetWithJS js t m =
  ( AppWidget js t m
  , HasJSContext (Performable m)
  , MonadJSM (Performable m)
  , MonadJSM m
  )

data State = NotStarted | Loading | Loaded (Maybe Int64)
  deriving (Eq)

data PostType = Comment Id Post | Thread

postAttrs :: State -> M.Map Text Text
postAttrs = \case
  Loading -> ("disabled" =: "true")
  _ -> mempty

postInput :: AppWidget js t m => PostType -> m (Dynamic t State)
postInput pt = do
  rec
    inputEl <- textAreaElement def
    (postBtn, _) <- elAttr "span" ("id" =: "post") $
      elDynAttr' "button" (postAttrs <$> state) $ text "create new thread"
    let click = domEvent Click postBtn
    let post = tag (current $ _textAreaElement_value inputEl) click
    request <- prerender
      case pf of
        Comment postId ->
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync (commentRequest <$> post postId))
        Thread ->
          (pure never)
          (fmap decodeXhrResponse <$> performRequestAsync (threadRequest <$> post))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
  pure state

threadRoute :: Text
threadRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewTh :/ ()
commentRoute :: Text
commentRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewCom :/ ()
listRoute :: Text
listRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_ListPosts :/ ()

threadRequest :: Text -> XhrRequest Text
threadRequest s = postJson threadRoute (PostRequest True s Nothing) --forcing image true until I actually figure it out

commentRequest :: Text -> Id Post -> XhrRequest Text
commentRequest s postId = postJson commentRoute (PostRequest False s (Just postId)) --forcing image false

viewPlaceholder :: AppWidget js t m => m () -- loading screen
viewPlaceholder = text "Loading post..."

viewWholeThread :: WidgetWithJS js t m => Dynamic t (Id Post) -> m () -------finish this
viewWholeThread postId = do
  onload <- getPostBuild
  initialLoad <- getAndDecode $ getPostUrl <$> tag (current postId) onload
  widgetHold_ viewPlaceholder (showPost Result <$> initialLoad)

stateToMaybeKey :: State -> Maybe Int64
stateToMaybeKey = \case
  Loaded key -> key
  _ -> Nothing



viewThreadList :: [PostResponse]
viewThreadList = postJson listRoute (PostFetch 0 20)-- need to make a type of typeclass to json to hold to ints
  

renderPost :: AppWidget js t m => PostResponse -> m ()
renderPost p = do
  text $ _postResponse_datetime p
  text $ _postResponse_content p

app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main -> do
      state <- postInput $ Thread
      prerender_
        viewPlaceholder -- loading screen
        (viewThreadList) --list of posts
      setRoute $ (FrontendRoute_ViewPost :/) . Id <$> fmapMaybe id (stateToMaybeKey <$> updated state)
    FrontendRoute_ViewPost -> do
      postId <- askRoute
      --BackendRoute_GetPost :/ postId will return list of posts
      --case first post id == postId and Op is Nothing
        --true -> postId is thread op, format thread like regular
        --false -> postId is a comment in the thread
                --on load, scroll to comment with id postId and highlight it
      state <- postInput $ Comment postId
      prerender_
        viewPlaceholder -- loading screen
        (viewWholeThread postId) --thread
-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "OBchan"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = app
  }
