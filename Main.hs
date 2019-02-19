{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad (void)
import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import qualified Data.Text as Text

data PlayState = Playing | Paused

data PlaylistState = PlaylistState
  { playlistFiles     :: [FilePath]
  , playlistVolume    :: Double
  , playlistProgress  :: Double
  , playlistPlayState :: PlayState
  }

defaultPlaylistState :: [FilePath] -> PlaylistState
defaultPlaylistState files = PlaylistState
  { playlistFiles     = files
  , playlistVolume    = 0.0
  , playlistProgress  = 0.0 -- TODO(DarinM223): check that beginning is at 0.0.
  , playlistPlayState = Paused
  }

data State = ChooseFolder | Playlist PlaylistState

data PlayEvent = TogglePlay
               | NextButtonClicked
               | PrevButtonClicked
               | SetVolume Double
               | UpdateProgress Double
               | BackToChooseFolder

data Event = FolderEvent (Either String [FilePath])
           | PlayEvent PlayEvent
           | Closed

data FolderNotFound = FolderNotFound deriving Show
instance Exception FolderNotFound

soundFileExts :: [String]
soundFileExts = [".wav", ".mp3"]

chooseFolderWidget :: Widget (Either String [FilePath])
chooseFolderWidget = container Box
  [#orientation := OrientationHorizontal]
  [ BoxChild defaultBoxChildProperties $
    widget Label [#label := "Choose folder:"]
  , BoxChild defaultBoxChildProperties
    { expand = True, fill = True, padding = 10 } $
    widget FileChooserButton
    [ #title := "Choose folder"
    , #action := FileChooserActionSelectFolder
    , onM #selectionChanged onSelectionChanged
    ]
  ]
 where
  onSelectionChanged chooser = do
    filename <- fileChooserGetFilename chooser
    toEvent <$> maybe err (try . listDirectory) filename
   where
    err = pure $ Left FolderNotFound
    toEvent (Left e)      = Left $ displayException e
    toEvent (Right files) =
      Right $ filter ((`elem` soundFileExts) . takeExtension) files

playWidget :: PlaylistState -> Widget PlayEvent
playWidget s = container Box
  [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties $ widget Scale []
  , BoxChild defaultBoxChildProperties $ container Box
    [#orientation := OrientationHorizontal]
    [ BoxChild playButtonProps $ widget Button []
    , BoxChild playButtonProps $ widget Button []
    , BoxChild playButtonProps $ widget Button []
    ]
  , BoxChild defaultBoxChildProperties $ widget VolumeButton []
  ]
 where
  playButtonProps = defaultBoxChildProperties
    { expand = True, fill = True, padding = 10 }

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := "Folder Music Player"
  , on #deleteEvent (const (True, Closed))
  , #widthRequest := 400
  , #heightRequest := 70
  ] $
  case s of
    ChooseFolder  -> FolderEvent <$> chooseFolderWidget
    Playlist state -> PlayEvent <$> playWidget state

update' :: State -> Event -> Transition State Event
update' ChooseFolder (FolderEvent (Right ps)) =
  Transition (Playlist $ defaultPlaylistState ps) (Nothing <$ print ps)
update' ChooseFolder (FolderEvent (Left e)) =
  Transition ChooseFolder (Nothing <$ putStrLn e)
update' _ Closed = Exit
update' _ _ = Transition ChooseFolder (pure Nothing)

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = ChooseFolder
  }
