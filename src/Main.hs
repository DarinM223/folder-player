{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import GI.Gtk.Declarative.App.Simple
import Window (State (..), update', view')
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified SDL.Mixer as Mixer

styles :: ByteString
styles = mconcat
  [ ".error-button { color: red; }"
  , ".group-button { border-radius: 50%; }"
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
