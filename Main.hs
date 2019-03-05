{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Exception
import Control.Monad (void)
import Control.Monad.Loops (whileM_)
import Data.Bifunctor (first)
import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified SDL.Mixer as Mixer

data PlayState = Playing | Paused

data MusicInfo = MusicInfo deriving (Show, Eq)

data MusicFile = MusicFile
  { musicFilePath :: FilePath
  , musicFileInfo :: MusicInfo
  } deriving (Show, Eq)

-- TODO(DarinM223): load file info from path
mkMusicFile :: FilePath -> IO MusicFile
mkMusicFile path = return MusicFile
  { musicFilePath = path
  , musicFileInfo = MusicInfo
  }

playMusic :: TMVar PlayEvent -> MusicFile -> IO (Maybe PlayEvent)
playMusic interrupt file = do
  music <- Mixer.load $ musicFilePath file
  Mixer.playMusic 1 music
  finished <- newEmptyTMVarIO
  forkIO $ do
    whileM_ Mixer.playingMusic $ threadDelay 50
    atomically $ putTMVar finished NextButtonClicked
  action <- atomically $ takeTMVar interrupt <|> takeTMVar finished
  Mixer.free music
  return $ Just action

data PlaylistState = PlaylistState
  { playlistFiles     :: V.Vector MusicFile
  , playlistCurrIndex :: Int
  , playlistInterrupt :: Maybe (TMVar PlayEvent)
  , playlistVolume    :: Int
  , playlistPlayState :: PlayState
  }

defaultPlaylistState :: [MusicFile] -> PlaylistState
defaultPlaylistState files = PlaylistState
  { playlistFiles     = V.fromList files
  , playlistCurrIndex = 0
  , playlistInterrupt = Nothing
  , playlistVolume    = 0
  , playlistPlayState = Paused
  }

adjustIndex :: PlaylistState -> Int -> Int
adjustIndex s amount =
  (playlistCurrIndex s + amount) `mod` V.length (playlistFiles s)

data State = ChooseFolder | Playlist PlaylistState

newtype Interrupt event = Interrupt (TMVar event) deriving Eq
instance Show (Interrupt event) where
  show _ = ""

data PlayEvent = TogglePlay
               | NextButtonClicked
               | PrevButtonClicked
               | SetInterrupt (Interrupt PlayEvent)
               | SetVolume Int
               | BackToChooseFolder
               deriving (Show, Eq)

data Event = FolderEvent (Either String [MusicFile])
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

chooseFolderWidget :: Widget (Either String [MusicFile])
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
    paths <- toEvent dir <$> maybe (pure err) (try . listDirectory) dir
    either (pure . Left) (fmap Right . mapM mkMusicFile) paths
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
  [ BoxChild groupProps $ container Box
    [#orientation := OrientationHorizontal]
    [ BoxChild playButtonProps $ widget Button
      [#label := "<<", on #clicked PrevButtonClicked]
    , BoxChild playButtonProps $ widget Button
      [#label := ">", on #clicked TogglePlay]
    , BoxChild playButtonProps $ widget Button
      [#label := ">>", on #clicked NextButtonClicked]
    ]
  , BoxChild defaultBoxChildProperties $ widget VolumeButton
    [on #valueChanged onVolumeChanged]
  ]
 where
  groupProps = defaultBoxChildProperties { expand = True, fill = True }
  playButtonProps = defaultBoxChildProperties
    { expand = True, fill = True, padding = 10 }
  onVolumeChanged =
    SetVolume . floor . (* fromIntegral mixerMaxVolume)

putInterrupt :: PlaylistState -> IO (Maybe PlayEvent)
putInterrupt s = do
  i <- Interrupt <$> newEmptyTMVarIO
  mapM_ (put i) (playlistInterrupt s)
  return Nothing
 where put i var = atomically $ putTMVar var $ SetInterrupt i

updatePlay :: PlaylistState -> PlayEvent -> Transition PlaylistState PlayEvent
updatePlay s TogglePlay = case playlistPlayState s of
  Playing -> Transition
    s { playlistPlayState = Paused }
    (Nothing <$ Mixer.pauseMusic)
  Paused -> Transition (s { playlistPlayState = Playing }) $ do
    paused <- Mixer.pausedMusic
    i <- Interrupt <$> newEmptyTMVarIO
    if | paused                         -> Nothing <$ Mixer.resumeMusic
       | not (V.null (playlistFiles s)) -> return $ Just $ SetInterrupt i
       | otherwise                      -> return Nothing
updatePlay s (SetInterrupt (Interrupt var)) = Transition
  s { playlistInterrupt = Just var }
  (playMusic var $ playlistFiles s V.! playlistCurrIndex s)
updatePlay s PrevButtonClicked = Transition
  s { playlistCurrIndex = prevIndex }
  (putInterrupt s)
 where prevIndex = adjustIndex s (-1)
updatePlay s NextButtonClicked = Transition
  s { playlistCurrIndex = nextIndex }
  (putInterrupt s)
 where nextIndex = adjustIndex s 1
updatePlay s (SetVolume volume) = Transition
  s { playlistVolume = volume }
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
