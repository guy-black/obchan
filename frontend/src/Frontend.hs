{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes#-}

module Frontend where

import Control.Monad
import GHC.Int
import Database.Id.Class (Id(..))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text as T
import Data.Maybe (listToMaybe)
import Data.List
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM, eval, liftJSM, fromJSVal, toJSVal)
import Data.Time as Time --for timestamp in postresponse
import GHCJS.DOM.Types (File)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.FileReader (newFileReader, readAsDataURL, load, getResult)
import qualified Snap.Core as S
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location

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

fileInputElement :: DomBuilder t m => m (Dynamic t [File])
fileInputElement = do
  ie <- inputElement $ def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("id" =: "imgUpload" <> "type" =: "file" <> "accept" =: "image/png, image/jpeg")
  return (_inputElement_files ie)

dataURLFileReader
  :: ( DomBuilder t m
     , TriggerEvent t m
     , PerformEvent t m
     , Prerender js t m
     )
  => Event t File -> m (Event t Text)
dataURLFileReader request = do
  readD <- prerender (return never) $ do
    fileReader <- liftJSM newFileReader
    performEvent_ (fmap (readAsDataURL fileReader . Just) request)
    e <- wrapDomEvent fileReader (`on` load) . liftJSM $ do
      v <- getResult fileReader
      (fromJSVal <=< toJSVal) v
    return (fmapMaybe id e)
  return (coincidence (updated readD))

inputBox :: AppWidget js t m => Event t () -> m (Event t (Text,Text))
inputBox clr = do
  divClass "input" $ do
    inputEl <- textAreaElement def { _textAreaElementConfig_setValue = Just ("" <$ clr)}
    filesDyn <- fileInputElement
    urlE <- -- fmap (ffilter ("data:image" `T.isPrefixOf`)) .
         dataURLFileReader
        . fmapMaybe listToMaybe
        . updated $ filesDyn
    urlD <- holdDyn "initial" urlE
    dynText urlD
    display (Data.List.length <$> filesDyn)
    click <- button "submit"
    return (attach (current $ urlD) (tag (current $ _textAreaElement_value inputEl) click))

--shortcuts for the routes were gonnna use
threadRoute :: Text
threadRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewTh :/ ()
commentRoute :: Text
commentRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewCom :/ ()
listRoute :: Text
listRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_ListPosts :/ ()
postRoute :: Id PostResponse -> Text
postRoute postId = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_GetPost :/ postId

--routes we postJson to
threadRequest :: (Text,Text) -> XhrRequest Text
threadRequest (img, con) = postJson threadRoute (PostRequest img con Nothing) --forcing image true until I actually figure it out
commentRequest :: Id PostResponse -> (Text,Text) -> XhrRequest Text
commentRequest postId (img, con) = postJson commentRoute (PostRequest img con (Just postId)) --forcing image false
fetchThreads :: PostFetch ->  XhrRequest Text
fetchThreads pf= postJson listRoute pf

fromMaybe :: a -> Maybe a -> a
fromMaybe def m = case m of
  Nothing -> def
  Just j -> j


----loading screen
viewPlaceholder :: AppWidget js t m => m ()
viewPlaceholder = text "Loading post..."

viewPlaceholder' :: AppWidget js t m => m (Event t a)
viewPlaceholder' = do
  text "Loading post..."
  return (never)

--render functions
----render a thread preview for them main page
renderOp :: AppWidget js t m => ThreadResponse -> m ()
renderOp (ThreadResponse op rep cc) =
  elAttr "a" ("href" =: ("/p/" <> ((T.pack . show) (_postResponse_id op))) <> "class" =: "link") $
    elAttr "div" ("class" =: "op") $ do
      renderComment op
      text (((T.pack . show) cc) <> " comments")
      recRender renderComment (Prelude.take 3 rep) --ensure only 3 replies are rendered

--  elAttr "a" ("href" =: ("/p/" <> ((T.pack . show) (_postResponse_id po))) <> "class" =: "link") $
--    elAttr "div" ("class" =: "op" <> "id" =: ((T.pack . show) (_postResponse_id po))) $ do
--      elAttr "p" ("class" =: "timestamp") $ display $ (constDyn (_postResponse_datetime po))
--      elAttr "p" ("class" =: "postContent") $ text $ (fromMaybe "" (_postResponse_content po))
--      elAttr "p" ("class" =: "postId") $ display $ (constDyn (_postResponse_id po))


renderComment :: AppWidget js t m => PostResponse -> m ()
renderComment po =
  case (_postResponse_image po) of
    "" ->
      elAttr "div" ("class" =: "comment" <> "id" =: ((T.pack . show) (_postResponse_id po))) $ do
        elAttr "p" ("class" =: "timestamp") $ display $ (constDyn (_postResponse_datetime po))
        elAttr "p" ("class" =: "postContent") $ text $ _postResponse_content po
        elAttr "p" ("class" =: "postId") $ display $ (constDyn (_postResponse_id po))
    im ->
      elAttr "div" ("class" =: "comment" <> "id" =: ((T.pack . show) (_postResponse_id po))) $ do
        elAttr "img" ("src" =: im <> "style" =: "max-width: 40%") blank
        elAttr "p" ("class" =: "timestamp") $ display $ (constDyn (_postResponse_datetime po))
        elAttr "p" ("class" =: "postContent") $ text $ _postResponse_content po
        elAttr "p" ("class" =: "postId") $ display $ (constDyn (_postResponse_id po))

recRender :: AppWidget js t m => (a -> m ()) -> [a] -> m ()
recRender rf tl =
  case tl of
    [] -> blank
    [x] -> rf x
    x : xs -> do
      rf x
      recRender rf xs

renderThread :: WidgetWithJS js t m => Dynamic t (Id PostResponse) -> Event t () -> m(Event t (Maybe (Id PostResponse)))
renderThread postId mo = do
  loaded <- getPostBuild
  let evMoreThr = leftmost [loaded, mo]
  evMaLiPr <- getAndDecode $ postRoute <$> tag (current postId) evMoreThr
  let evLiPr = (fmapMaybe id evMaLiPr)
  divClass "content" $ do
    widgetHold_ (text "loading thread") ((recRender renderComment) <$> evLiPr)
  return (firstId <$> evLiPr)

renderMain :: WidgetWithJS js t m => Event t () -> m ()
renderMain mo = do
  loaded <- getPostBuild
  let evMoreThr = leftmost [loaded, mo] --Event t ()
  currPF <-  foldDyn (\_ (PostFetch o c) -> PostFetch (o + 15) c ) (PostFetch 0 15) evMoreThr
  let taggedEvThr = tag (current currPF) evMoreThr --Event t (PostFetch), later use foldDyn to increment pf
  evXhrRes <- performRequestAsync (fetchThreads <$> taggedEvThr)
  let evLiPr = fmapMaybe id (decodeXhrResponse <$> evXhrRes)
  dynLiPr <- foldDyn revCon [] evLiPr
  divClass "content" $ do
    dyn_ ((recRender renderOp) <$> dynLiPr)

revCon :: [a] -> [a] -> [a]
revCon a b = b ++ a

firstId :: [PostResponse] -> Maybe (Id PostResponse)
firstId l =
  case l of
    p:ps -> Just (Id (_postResponse_id p))
    _ -> Nothing


rightRoute :: Int64 -> Int64 -> Text
rightRoute com op =
  ("/p/" <> ((T.pack . show) op) <> "#" <> ((T.pack . show) com))

jsReload :: forall a m t js. (WidgetWithJS js t m, Monad m) => Text -> m ()
jsReload t = prerender_ blank $ liftJSM $ void $ eval t

jsNoOp :: forall m t js. (WidgetWithJS js t m, Monad m) => m ()
jsNoOp = prerender_ blank $ liftJSM $ void $ eval ("" :: T.Text)



app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main -> do
      evImgCon <- inputBox never
      evThRes <- prerender
        (return never)
        (performRequestAsync (threadRequest <$> evImgCon))
      rec
        prerender_
          (text "loading")
          (renderMain evMoreThr)
        evMoreThr <- button "see more"
      setRoute $ (FrontendRoute_ViewPost :/) . Id <$> fmapMaybe id (decodeXhrResponse <$> (switchDyn evThRes))
    FrontendRoute_ViewPost -> do
      postId <- askRoute
      rec
        evImgCon <- inputBox evComPosted
        dynEvComRes <- prerender
          (pure never)
          (performRequestAsync ((commentRequest <$> (current postId)) <@> evImgCon))
        let evComPosted = void (switchDyn dynEvComRes)
      redirect <- prerender -- redirect :: Dynamic t (Event t (Maybe (Id PostRresponse)))
        viewPlaceholder' -- loading screen
        (renderThread postId evComPosted)
      pid <- (sample . current) postId
      --setRoute $ (FrontendRoute_Main :/) <$> evComPosted -- if comment posted redirect to home
      let evRecPostId = fmapMaybe id (switchDyn redirect) -- Event t (Id PostResponse), the Id of Op of thread
      setRoute $ (FrontendRoute_ViewPost :/) <$> (ffilter (pid /=) evRecPostId) -- redirect to right thread

header :: AppWidget js t m => m()
header =
  divClass "header" $ do
    elAttr "a" ("href" =: "/" <> "class" =: "header") $
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
