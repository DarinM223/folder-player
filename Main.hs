{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Exception
import Control.Monad (void)
import Control.Monad.Loops (whileM_)
import Data.ByteString (ByteString)
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe, isJust)
import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified SDL.Mixer as Mixer
import qualified Sound.HTagLib as Tag

data PlayState = Playing | Paused

data MusicInfo = MusicInfo
  { infoTitle  :: Tag.Title
  , infoArtist :: Tag.Artist
  , infoAlbum  :: Tag.Album
  , infoYear   :: Maybe Tag.Year
  } deriving (Show, Eq)

musicInfoGetter :: Tag.TagGetter MusicInfo
musicInfoGetter = MusicInfo
  <$> Tag.titleGetter
  <*> Tag.artistGetter
  <*> Tag.albumGetter
  <*> Tag.yearGetter

data MusicFile = MusicFile
  { musicFilePath :: FilePath
  , musicFileInfo :: MusicInfo
  } deriving (Show, Eq)

mkMusicFile :: FilePath -> IO MusicFile
mkMusicFile path = MusicFile <$> pure path <*> Tag.getTags path musicInfoGetter

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

adjustIndex :: PlaylistState -> Int -> Int
adjustIndex s amount =
  (playlistCurrIndex s + amount) `mod` V.length (playlistFiles s)

data State = ChooseFolder (Maybe Text.Text) | Playlist PlaylistState

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

data Event = FolderEvent (Either Text.Text (Int, [MusicFile]))
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

chooseFolderWidget :: Maybe Text.Text
                   -> Widget (Either Text.Text (Int, [MusicFile]))
chooseFolderWidget err = container Box
  [#orientation := OrientationVertical]
  [ BoxChild defaultBoxChildProperties $ widget Label
    [ classes ["error-button"]
    , #label := fromMaybe "" err
    , #visible := isJust err
    ]
  , BoxChild defaultBoxChildProperties $ container Box
    [#orientation := OrientationHorizontal]
    [ BoxChild defaultBoxChildProperties $
      widget Label [#label := "Choose first music file to play in folder:"]
    , BoxChild defaultBoxChildProperties
      { expand = True, fill = True, padding = 10 } $
      widget FileChooserButton
      [ #title := "Choose first music file in folder"
      , onM #selectionChanged onSelectionChanged
      ]
    ]
  ]
 where
  onSelectionChanged chooser = fileChooserGetFilename chooser >>= \case
    Just file | takeExtension file `elem` soundFileExts-> do
      let dir = takeDirectory file
      paths <- keepSoundFiles dir <$> try @IOError (listDirectory dir)
      let i = either (const 0) (fromMaybe 0 . elemIndex file) paths
      either (pure . Left) (fmap (Right . (i, )) . mapM mkMusicFile) paths
    _ -> return $ Left "No music file selected"
   where
    keepSoundFiles _ (Left e) = Left $ Text.pack $ displayException e
    keepSoundFiles dir (Right files)
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
      [#label := "|>", onM #clicked onPlayClicked]
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
  onPlayClicked button = do
    buttonGetLabel button >>= \case
      "|>" -> buttonSetLabel button "||"
      "||" -> buttonSetLabel button "|>"
      _    -> return ()
    return TogglePlay

putInterrupt :: PlaylistState -> IO (Maybe PlayEvent)
putInterrupt s = do
  i <- Interrupt <$> newEmptyTMVarIO
  success <- mapM (put i) (playlistInterrupt s)
  if success == Just True
    then return Nothing
    else return $ Just $ SetInterrupt i
 where put i var = atomically $ tryPutTMVar var $ SetInterrupt i

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
  s { playlistCurrIndex = adjustIndex s (-1) }
  (putInterrupt s)
updatePlay s NextButtonClicked = Transition
  s { playlistCurrIndex = adjustIndex s 1 }
  (putInterrupt s)
updatePlay s (SetVolume volume) = Transition
  s { playlistVolume = volume }
  (Nothing <$ Mixer.setMusicVolume volume)
updatePlay s StopPlaying = Transition s (pure Nothing)
updatePlay s p = Transition s (Nothing <$ print p)

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := title
  , on #deleteEvent (const (True, Closed))
  , #widthRequest := 400
  , #heightRequest := 70
  ] $
  case s of
    ChooseFolder e -> FolderEvent <$> chooseFolderWidget e
    Playlist state -> PlayEvent <$> playWidget state
 where
  currPlayingFile ps = Text.pack $ takeFileName $ musicFilePath file
   where file = playlistFiles ps V.! playlistCurrIndex ps
  title = case s of
    Playlist ps@PlaylistState { playlistInterrupt = Just _ } ->
      "Playing " <> currPlayingFile ps
    _ -> "Folder Music Player"

update' :: State -> Event -> Transition State Event
update' (ChooseFolder _) (FolderEvent (Right (i, ps))) =
  Transition (Playlist $ defaultPlaylistState i ps) (pure Nothing)
update' (ChooseFolder _) (FolderEvent (Left e)) =
  Transition (ChooseFolder (Just e)) (pure Nothing)
update' (Playlist s) (PlayEvent p) =
  mapTransition Playlist PlayEvent $ updatePlay s p
update' s Closed = case s of
  Playlist ps ->
    Transition (ChooseFolder Nothing) $ do
      mapM_ (atomically . flip tryPutTMVar StopPlaying) (playlistInterrupt ps)
      return Nothing
  _ -> Exit
update' _ _ = Transition (ChooseFolder Nothing) (pure Nothing)

styles :: ByteString
styles = mconcat
  [ ".error-button { color: red; }"
  ]

main :: IO ()
main = initAudio $ const $ void $ do
  Mixer.setMusicVolume 0
  void $ Gtk.init Nothing
  screen <- fromJust <$> Gdk.screenGetDefault
  p <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
  void $ forkIO $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
 where
  initAudio = bracket
    (Mixer.openAudio Mixer.defaultAudio 4096)
    (const Mixer.closeAudio)
  app = App { view         = view'
            , update       = update'
            , inputs       = []
            , initialState = ChooseFolder Nothing }
