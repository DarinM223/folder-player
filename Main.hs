{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad (void)
import Data.Bifunctor (first)
import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import qualified Data.Text as Text
import qualified SDL.Mixer as Mixer

data PlayState = Playing | Paused

data PlaylistState = PlaylistState
  { playlistFiles     :: [FilePath]
  , playlistVolume    :: Int
  , playlistProgress  :: Double
  , playlistPlayState :: PlayState
  }

defaultPlaylistState :: [FilePath] -> PlaylistState
defaultPlaylistState files = PlaylistState
  { playlistFiles     = files
  , playlistVolume    = 0
  , playlistProgress  = 0.0 -- TODO(DarinM223): check that beginning is at 0.0.
  , playlistPlayState = Paused
  }

data State = ChooseFolder | Playlist PlaylistState

data PlayEvent = TogglePlay
               | NextButtonClicked
               | PrevButtonClicked
               | SetVolume Int
               | UpdateProgress Double
               | BackToChooseFolder
               deriving (Show, Eq)

data Event = FolderEvent (Either String [FilePath])
           | PlayEvent PlayEvent
           | Closed
           deriving (Show, Eq)

data FolderNotFound = FolderNotFound deriving Show
instance Exception FolderNotFound

mixerMaxVolume :: Int
mixerMaxVolume = 128

soundFileExts :: [String]
soundFileExts = [".wav", ".mp3"]

mapTransition :: (s -> s') -> (e -> e') -> Transition s e -> Transition s' e'
mapTransition _ _ Exit = Exit
mapTransition mapS mapE (Transition s me) =
  Transition (mapS s) (fmap mapE <$> me)

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
    dir <- fileChooserGetFilename chooser
    toEvent dir <$> maybe (pure err) (try . listDirectory) dir
   where
    err = Left FolderNotFound
    toEvent Nothing _  = first displayException err
    toEvent _ (Left e) = Left $ displayException e
    toEvent (Just dir) (Right files)
      = Right
      . fmap (dir </>)
      . filter ((`elem` soundFileExts) . takeExtension)
      $ files

playWidget :: PlaylistState -> Widget PlayEvent
playWidget s = container Box
  [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties $ widget Scale []
  , BoxChild defaultBoxChildProperties $ container Box
    [#orientation := OrientationHorizontal]
    [ BoxChild playButtonProps $ widget Button [on #clicked PrevButtonClicked]
    , BoxChild playButtonProps $ widget Button [on #clicked TogglePlay]
    , BoxChild playButtonProps $ widget Button [on #clicked NextButtonClicked]
    ]
  , BoxChild defaultBoxChildProperties $ widget VolumeButton
    [on #valueChanged onVolumeChanged]
  ]
 where
  playButtonProps = defaultBoxChildProperties
    { expand = True, fill = True, padding = 10 }
  onVolumeChanged =
    SetVolume . floor . (* fromIntegral mixerMaxVolume)

updatePlay :: PlaylistState -> PlayEvent -> Transition PlaylistState PlayEvent
updatePlay s TogglePlay = case playlistPlayState s of
  Playing -> Transition (s { playlistPlayState = Paused }) $
    Nothing <$ Mixer.pauseMusic
  Paused -> Transition (s { playlistPlayState = Playing }) $ do
    paused <- Mixer.pausedMusic
    case (paused, playlistFiles s) of
      (True, _)    -> Nothing <$ Mixer.resumeMusic
      (False, f:_) -> do
        music <- Mixer.load f
        Mixer.playMusic 1 music
        -- TODO(DarinM223): create thread that checks if complete
        return Nothing
      _ -> return Nothing
updatePlay s (SetVolume volume) = Transition
  (s { playlistVolume = volume })
  (Nothing <$ Mixer.setMusicVolume volume)
updatePlay s p = Transition s (Nothing <$ print p)

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := "Folder Music Player"
  , on #deleteEvent (const (True, Closed))
  , #widthRequest := 400
  , #heightRequest := 70
  ] $
  case s of
    ChooseFolder   -> FolderEvent <$> chooseFolderWidget
    Playlist state -> PlayEvent <$> playWidget state

update' :: State -> Event -> Transition State Event
update' ChooseFolder (FolderEvent (Right ps)) =
  Transition (Playlist $ defaultPlaylistState ps) (pure Nothing)
update' ChooseFolder (FolderEvent (Left e)) =
  Transition ChooseFolder (Nothing <$ putStrLn e)
update' (Playlist s) (PlayEvent p) =
  mapTransition Playlist PlayEvent $ updatePlay s p
update' _ Closed = Exit
update' _ _ = Transition ChooseFolder (pure Nothing)

main :: IO ()
main = initAudio $ const $ void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = ChooseFolder
  }
 where
  initAudio = bracket
    (Mixer.openAudio Mixer.defaultAudio 4096)
    (const Mixer.closeAudio)
