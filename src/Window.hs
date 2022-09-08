{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Window
  ( State (..)
  , view'
  , update'
  ) where

import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.FilePath (takeFileName)
import ChooseFolder (chooseFolderWidget)
import MusicFile (MusicFile, musicFilePath)
import Playlist
import qualified Data.Text as T
import qualified Data.Vector as V

data State = ChooseFolder (Maybe T.Text) | Playlist PlaylistState

data Event = FolderEvent (Either T.Text (Int, [MusicFile]))
           | InitState PlaylistState
           | PlayEvent PlayEvent
           | Closed
           deriving (Show, Eq)

mapTransition :: (s -> s') -> (e -> e') -> Transition s e -> Transition s' e'
mapTransition _ _ Exit = Exit
mapTransition mapS mapE (Transition s me) =
  Transition (mapS s) (fmap mapE <$> me)

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := title
  , on #deleteEvent (const (True, Closed))
  , #widthRequest := 400
  , #heightRequest := 100
  ] $
  case s of
    ChooseFolder e -> FolderEvent <$> chooseFolderWidget e
    Playlist state -> PlayEvent <$> playlistWidget state
 where
  currPlayingFile ps = T.pack $ takeFileName $ musicFilePath file
   where file = playlistFiles ps V.! playlistCurrIndex ps
  title = case s of
    Playlist ps@PlaylistState { playlistPlayState = Playing } ->
      "Playing " <> currPlayingFile ps
    _ -> "Folder Music Player"

update' :: State -> Event -> Transition State Event
update' (ChooseFolder _) (FolderEvent (Right (i, ps))) =
  Transition (ChooseFolder Nothing) $ Just . InitState <$> mkPlaylistState i ps
update' (ChooseFolder _) (FolderEvent (Left e)) =
  Transition (ChooseFolder (Just e)) (pure Nothing)
update' (Playlist s) (PlayEvent p) =
  mapTransition Playlist PlayEvent $ updatePlaylist s p
update' _ (InitState state) = Transition (Playlist state) (pure Nothing)
update' s Closed = case s of
  Playlist ps ->
    Transition (ChooseFolder Nothing) $ Nothing <$ interruptMusic ps StopPlaying
  _ -> Exit
update' _ _ = Transition (ChooseFolder Nothing) (pure Nothing)
