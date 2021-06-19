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

data PostType = Comment (Id Post) | Thread
--helper functions
postAttrs :: State -> M.Map Text Text
postAttrs = \case
  Loading -> ("disabled" =: "true")
  _ -> mempty
stateToMaybeKey :: State -> Maybe Int64
stateToMaybeKey = \case
  Loaded key -> key
  _ -> Nothing

--thread input widget
threadInput :: AppWidget js t m => m (Dynamic t State)
threadInput = do
  rec
    inputEl <- textAreaElement def
    (postBtn, _) <- elAttr "span" ("id" =: "post") $
      elDynAttr' "button" (postAttrs <$> state) $ text "start a thread"
    let click = domEvent Click postBtn
    let post = tag (current $ _textAreaElement_value inputEl) click
    request <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (threadRequest <$> post))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
  pure state

--comment input widget
commentInput :: AppWidget js t m => (Id Post) -> m (Dynamic t State)
commentInput postId = do
  rec
    inputEl <- textAreaElement def
    (postBtn, _) <- elAttr "span" ("id" =: "post") $
      elDynAttr' "button" (postAttrs <$> state) $ text "add a comment"
    let click = domEvent Click postBtn
    let post = tag (current $ _textAreaElement_value inputEl) click
    request <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync ((commentRequest postId) <$> post))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
  pure state

--shortcuts for the routes were gonnna use
threadRoute :: Text
threadRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewTh :/ ()
commentRoute :: Text
commentRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewCom :/ ()
listRoute :: Text
listRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_ListPosts :/ ()
postRoute :: Id Post -> Text
postRoute postId = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_GetPost :/ postId
--routes we postJson to
threadRequest :: Text -> XhrRequest Text
threadRequest s = postJson threadRoute (PostRequest True (Just s) Nothing) --forcing image true until I actually figure it out
commentRequest :: Id Post -> Text -> XhrRequest Text
commentRequest postId s = postJson commentRoute (PostRequest False (Just s) (Just postId)) --forcing image false
fetchThreads :: PostFetch ->  XhrRequest Text 
fetchThreads pf= postJson listRoute pf --pf 0 20
--decode functions
decodeThreadList :: PostFetch -> [Maybe PostResponse]
decodeThreadList pf = decodeXhrResponse <$> performRequestAsync (fetchThreads <$> pf) --need to wrap pf in an event
decodeThread :: Id Post -> [PostResponse]
decodeThread postId = getAndDecode $ postRoute postId
--render functions
----render an individual post
renderPost :: AppWidget js t m => Maybe PostResponse -> m ()
renderPost p = do
  case p of
    Just po ->
      text $ _postResponse_datetime p
      text $ _postResponse_content p
    _ -> text $ "uhhhhhh oops"
----loading screen
viewPlaceholder :: AppWidget js t m => m ()
viewPlaceholder = text "Loading post..."
----show a thread - OP and comments
------todo: check if postId matches first post in [postResponse], otherwise highlight comment with id postId
viewWholeThread :: AppWidget js t m => Id Post -> [PostResponse] -> m ()
viewWholeThread postId l = Map renderPost l
----show main list of threads
viewThreadList :: AppWidget js t m => [Maybe PostResponse] -> m ()
viewThreadList l = Map renderPost l

app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main -> do
      state <- postInput $ Thread
      prerender_
        viewPlaceholder -- loading screen
        (viewThreadList (decodeThreadList updated $ constDyn (PostFetch 0 20))) --list of posts
        --todo allow users to change page or number of posts per page
      setRoute $ (FrontendRoute_ViewPost :/) . Id <$> fmapMaybe id (stateToMaybeKey <$> updated state)
    FrontendRoute_ViewPost -> do
      postId <- askRoute
      state <- postInput $ Comment postId
      prerender_
        viewPlaceholder -- loading screen
        (viewWholeThread postId (decodeThread postId)) --post and comments
        --todo : idk but this just seems messy
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
