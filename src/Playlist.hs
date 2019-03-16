{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Playlist
  ( PlaylistState (..)
  , PlayState (..)
  , PlayEvent (..)
  , defaultPlaylistState
  , mixerMaxVolume
  , playlistWidget
  , updatePlaylist
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Monad.Loops (whileM_)
import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import MusicFile
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified SDL.Mixer as Mixer

newtype Interrupt event = Interrupt (TMVar event) deriving Eq
instance Show (Interrupt event) where
  show _ = ""

data PlayEvent = TogglePlay
               | NextButtonClicked
               | PrevButtonClicked
               | StopPlaying
               | SetInterrupt (Interrupt PlayEvent)
               | SetVolume Int
               | BackToChooseFolder
               deriving (Show, Eq)

data PlayState = Playing | Paused

data PlaylistState = PlaylistState
  { playlistFiles     :: V.Vector MusicFile
  , playlistCurrIndex :: Int
  , playlistInterrupt :: Maybe (TMVar PlayEvent)
  , playlistVolume    :: Int
  , playlistPlayState :: PlayState
  }

defaultPlaylistState :: Int -> [MusicFile] -> PlaylistState
defaultPlaylistState i files = PlaylistState
  { playlistFiles     = V.fromList files
  , playlistCurrIndex = i
  , playlistInterrupt = Nothing
  , playlistVolume    = 0
  , playlistPlayState = Paused
  }

adjustIndex :: PlaylistState -> Int -> PlaylistState
adjustIndex s amount = s { playlistCurrIndex = i' }
 where i' = (playlistCurrIndex s + amount) `mod` V.length (playlistFiles s)

playText :: T.Text
playText = "❙▶"

pauseText :: T.Text
pauseText = "❚❚"

mixerMaxVolume :: Int
mixerMaxVolume = 128

playlistWidget :: PlaylistState -> Widget PlayEvent
playlistWidget s = container Box
  [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties { padding = 10 } $
    widget HeaderBar
    [ #title := albumTitle (musicFileInfo file)
    , #subtitle := albumText (musicFileInfo file)
    ]
  , BoxChild groupProps $ container Box
    [#orientation := OrientationHorizontal]
    [ BoxChild playButtonProps $ widget Button
      [classes ["group-button"], #label := "⏮", on #clicked PrevButtonClicked]
    , BoxChild playButtonProps $ widget Button
      [classes ["group-button"], #label := playText, onM #clicked onPlayClicked]
    , BoxChild playButtonProps $ widget Button
      [classes ["group-button"], #label := "⏭", on #clicked NextButtonClicked]
    ]
  , BoxChild defaultBoxChildProperties $ widget VolumeButton
    [on #valueChanged onVolumeChanged]
  ]
 where
  file = playlistFiles s V.! playlistCurrIndex s
  groupProps = defaultBoxChildProperties { expand = True, fill = True }
  playButtonProps = defaultBoxChildProperties
    { expand = True, fill = True, padding = 10 }
  onVolumeChanged = SetVolume . floor . (* fromIntegral mixerMaxVolume)
  onPlayClicked button = do
    buttonGetLabel button >>= \label ->
      if | label == playText  -> buttonSetLabel button pauseText
         | label == pauseText -> buttonSetLabel button playText
         | otherwise          -> return ()
    return TogglePlay

putInterrupt :: PlaylistState -> IO (Maybe PlayEvent)
putInterrupt s = do
  i <- Interrupt <$> newEmptyTMVarIO
  success <- mapM (put i) (playlistInterrupt s)
  if success == Just True
    then return Nothing
    else return $ Just $ SetInterrupt i
 where put i var = atomically $ tryPutTMVar var $ SetInterrupt i

playMusic :: TMVar PlayEvent -> MusicFile -> IO (Maybe PlayEvent)
playMusic interrupt file = do
  music <- Mixer.load $ musicFilePath file
  Mixer.playMusic 1 music
  finished <- newEmptyTMVarIO
  forkIO $ do
    whileM_ Mixer.playingMusic $ threadDelay 50
    atomically $ putTMVar finished NextButtonClicked
  action <- atomically $ do
    action <- takeTMVar interrupt <|> takeTMVar finished
    -- Make sure interrupt is not empty, even if action was taken from
    -- finished.
    tryPutTMVar interrupt action
    return action
  Mixer.free music
  return $ Just action

updatePlaylist :: PlaylistState
               -> PlayEvent
               -> Transition PlaylistState PlayEvent
updatePlaylist s event = case event of
  TogglePlay -> case playlistPlayState s of
    Playing -> Transition
      s { playlistPlayState = Paused } (Nothing <$ Mixer.pauseMusic)
    Paused -> Transition (s { playlistPlayState = Playing }) $ do
      paused <- Mixer.pausedMusic
      i <- Interrupt <$> newEmptyTMVarIO
      if | paused                         -> Nothing <$ Mixer.resumeMusic
         | not (V.null (playlistFiles s)) -> return $ Just $ SetInterrupt i
         | otherwise                      -> return Nothing
  NextButtonClicked -> Transition (adjustIndex s 1) (putInterrupt s)
  PrevButtonClicked -> Transition (adjustIndex s (-1)) (putInterrupt s)
  SetInterrupt (Interrupt var) -> Transition
    s { playlistInterrupt = Just var }
    (playMusic var $ playlistFiles s V.! playlistCurrIndex s)
  SetVolume volume -> Transition
    s { playlistVolume = volume } (Nothing <$ Mixer.setMusicVolume volume)
  StopPlaying -> Transition s (pure Nothing)
  _ -> Transition s (Nothing <$ print event)
