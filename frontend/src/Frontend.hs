{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import GHC.Int
import Database.Id.Class (Id(..))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text as T
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)
import Data.Time as Time --for timestamp in postresponse
import qualified Snap.Core as S

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

{--
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
--}

{--thread input widget
threadInput :: AppWidget js t m => m (Dynamic t State)
threadInput = do
  rec
    inputEl <- textAreaElement def
    click <- button "start a thread"
    let post = tag (current $ _textAreaElement_value inputEl) click
    request <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync (threadRequest <$> post))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
  pure state --}

{--comment input widget
commentInput :: AppWidget js t m => (Id Post) -> m (Dynamic t State)
commentInput postId = do
  rec
    inputEl <- textAreaElement def
    click <- button "add a comment"
    let post = tag (current $ _textAreaElement_value inputEl) click
    request <- prerender
      (pure never)
      (fmap decodeXhrResponse <$> performRequestAsync ((commentRequest postId) <$> post))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
  pure state --}

inputBox :: AppWidget js t m => m (Event t Text)
inputBox = do
  inputEl <- textAreaElement def
  click <- button "submit"
  return (tag (current $ _textAreaElement_value inputEl) click)

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
fetchThreads pf= postJson listRoute pf

--decode functions
--decodeThreadList :: Event t PostFetch -> Event t (Maybe [PostResponse])
--decodeThreadList pf = decodeXhrResponse <$> performRequestAsync (fetchThreads <$> pf) --need to wrap pf in an event
--decodeThread :: Id Post -> [PostResponse]
--decodeThread postId = getAndDecode $ postRoute postId

fromMaybe :: a -> Maybe a -> a
fromMaybe def m = case m of
  Nothing -> def
  Just j -> j

--render functions
----render an individual post
renderPost :: AppWidget js t m => PostResponse -> m ()
renderPost po =
  elAttr "div" ("class" =: "post" <> "id" =: ((T.pack . show) (_postResponse_id po))) $ do
    display $ (constDyn (_postResponse_datetime po))
    text $ ((T.pack . show) (_postResponse_id po))
    text $ (fromMaybe "" (_postResponse_content po))

----loading screen
viewPlaceholder :: AppWidget js t m => m ()
viewPlaceholder = text "Loading post..."

----show a thread - OP and comments
------todo: check if postId matches first post in [postResponse], otherwise highlight comment with id postId
--viewWholeThread :: AppWidget js t m => Id Post -> [PostResponse] -> m ()
--viewWholeThread postId l = map renderPost l
----show main list of threads
viewPostList :: AppWidget js t m => Dynamic t [PostResponse] -> m ()
viewPostList dl = do--should end up with some dynamic widget listing post responses
  divClass "content" $ do
    dyn_ (recRender <$> dl)

recRender :: AppWidget js t m => [PostResponse] -> m ()
recRender tl =
  case tl of
    [] -> text "whoops recRender got an empty list"
    [x] -> renderPost x
    x : xs -> do
      renderPost x
      recRender xs

{-
getThreadList :: WidgetWithJS js t m => m(Dynamic t [PostResponse])
getThreadList = do
  loaded <- getPostBuild -- will have to change this when implementing paging
  evThreadRes <- performRequestAsync (tag (constant (fetchThreads (PostFetch 0 20))) loaded)
  holdDyn [] (fmapMaybe id (decodeXhrResponse <$> evThreadRes))
-}

renderThread :: WidgetWithJS js t m => Dynamic t (Id Post) -> m()
renderThread postId = do
  loaded <- getPostBuild
  evMaLiPr <- getAndDecode $ postRoute <$> tag (current postId) loaded
  divClass "content" $ do
    widgetHold_ (text "loading thread") (recRender <$> (fmapMaybe id evMaLiPr))


renderMain :: WidgetWithJS js t m => Event t () -> m ()
renderMain mo = do
  loaded <- getPostBuild
  let evMoreThr = leftmost [loaded, mo] --Event t ()
  let taggedEvThr = tag (constant (PostFetch 0 20)) evMoreThr --Event t (PostFetch), later use foldDyn to increment pf
  evXhrRes <- performRequestAsync (fetchThreads <$> taggedEvThr)
  let evLiPr = fmapMaybe id (decodeXhrResponse <$> evXhrRes)
  dynLiPr <- foldDyn (++) [] evLiPr
  viewPostList dynLiPr

app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main -> do
      evText <- inputBox
      evThRes <- prerender
        (return never)
        (performRequestAsync (threadRequest <$> evText))
      let evMoreThr = never --later change this to fire whenever I want more threads to load
      prerender_
        (text "loading")
        (renderMain evMoreThr)
      setRoute $ (FrontendRoute_ViewPost :/) . Id <$> fmapMaybe id (decodeXhrResponse <$> (switchDyn evThRes))
    FrontendRoute_ViewPost -> do
      postId <- askRoute
      evText <- inputBox
      evComRes <- prerender
        (pure never)
        (performRequestAsync ((commentRequest <$> (current postId)) <@> evText))
      --comments <- holdDyn [] (fmapMaybe id (switchDyn evComList))
      prerender_
        viewPlaceholder -- loading screen
        (renderThread postId)
      --setRoute $ (FrontendRoute_ViewPost :/) . Id <$> fmapMaybe id (decodeXhrResponse <$> (switchDyn evComRes))
        --(viewWholeThread postId (decodeThread postId)) --post and comments
        --todo : idk but this just seems messy

header :: AppWidget js t m => m()
header =
  elAttr "a" ("class" =: "header" <> "href" =: "/") $
    text "OBchan"

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "OBchan"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      header
      app
  }
