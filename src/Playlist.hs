{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Playlist
  ( PlaylistState (..)
  , PlayState (..)
  , PlayEvent (..)
  , mkPlaylistState
  , mixerMaxVolume
  , playlistWidget
  , interruptMusic
  , updatePlaylist
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Loops (whileM_)
import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import MusicFile
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified SDL.Mixer as Mixer

newtype Interrupt event = Interrupt { unInterrupt :: TMVar event }
  deriving Eq
instance Show (Interrupt event) where
  show _ = ""

data PlayEvent = TogglePlay
               | AutoNext
               | NextButtonClicked
               | PrevButtonClicked
               | StopPlaying
               | PlayNewMusic
               | SetVolume Int
               | BackToChooseFolder
               deriving (Show, Eq)

data PlayState = Playing | Paused deriving (Show, Eq)

data PlaylistState = PlaylistState
  { playlistFiles     :: V.Vector MusicFile
  , playlistCurrIndex :: Int
  , playlistInterrupt :: Interrupt PlayEvent
  , playlistVolume    :: Int
  , playlistPlayState :: PlayState
  } deriving (Show, Eq)

mkPlaylistState :: Int -> [MusicFile] -> IO PlaylistState
mkPlaylistState i files = do
  interrupt <- Interrupt <$> newEmptyTMVarIO
  volume <- Mixer.getMusicVolume
  return PlaylistState
    { playlistFiles     = V.fromList files
    , playlistCurrIndex = i
    , playlistInterrupt = interrupt
    , playlistVolume    = volume
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
    [#value := volume, on #valueChanged onVolumeChanged]
  ]
 where
  file = playlistFiles s V.! playlistCurrIndex s
  volume = fromIntegral (playlistVolume s) / fromIntegral mixerMaxVolume
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

playMusic :: PlaylistState -> IO (Maybe PlayEvent)
playMusic s =
  playMusicFile (playlistInterrupt s) $ playlistFiles s V.! playlistCurrIndex s

playMusicFile :: Interrupt PlayEvent -> MusicFile -> IO (Maybe PlayEvent)
playMusicFile (Interrupt interrupt) file =
  withMusic file $ \music -> do
    void $ atomically $ tryTakeTMVar interrupt
    Mixer.playMusic 1 music
    let autoNextWhenDone = do
          whileM_ Mixer.playingMusic $ threadDelay 50
          atomically $ writeTMVar interrupt AutoNext
    withAsync autoNextWhenDone $ \_ ->
      Just <$> atomically (takeTMVar interrupt)
 where
  withMusic f =
    bracket (Mixer.load (musicFilePath f)) ((Mixer.haltMusic >>) . Mixer.free)

interruptMusic :: PlaylistState -> PlayEvent -> IO ()
interruptMusic s event = do
  -- Resumes the song before it gets destroyed to
  -- prevent the mixer from getting stuck in a paused state.
  Mixer.resumeMusic
  atomically $ writeTMVar (unInterrupt (playlistInterrupt s)) event

updatePlaylist :: PlaylistState
               -> PlayEvent
               -> Transition PlaylistState PlayEvent
updatePlaylist s event = case event of
  TogglePlay -> case playlistPlayState s of
    Playing -> Transition
      s { playlistPlayState = Paused } (Nothing <$ Mixer.pauseMusic)
    Paused -> Transition (s { playlistPlayState = Playing }) $ do
      paused <- Mixer.pausedMusic
      if | paused                         -> Nothing <$ Mixer.resumeMusic
         | not (V.null (playlistFiles s)) -> playMusic s
         | otherwise                      -> return Nothing
  AutoNext -> Transition (adjustIndex s 1) (pure $ Just PlayNewMusic)
  NextButtonClicked ->
    Transition (adjustIndex s 1) (Nothing <$ interruptMusic s PlayNewMusic)
  PrevButtonClicked ->
    Transition (adjustIndex s (-1)) (Nothing <$ interruptMusic s PlayNewMusic)
  PlayNewMusic -> Transition s $
    if playlistPlayState s == Playing then playMusic s else pure Nothing
  SetVolume volume -> Transition
    s { playlistVolume = volume } (Nothing <$ Mixer.setMusicVolume volume)
  StopPlaying -> Transition s (pure Nothing)
  _ -> Transition s (Nothing <$ print event)
