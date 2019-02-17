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
import qualified Data.Text as Text

data State = State
data Event = FolderEvent (Either String [FilePath]) | Closed

data FolderNotFound = FolderNotFound deriving Show
instance Exception FolderNotFound

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
    toEvent (Right files) = Right files
    -- TODO(DarinM223): only select sound files

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := "Folder Music Player"
  , on #deleteEvent (const (True, Closed))
  , #widthRequest := 400
  , #heightRequest := 70
  ] $
  case s of
    State -> FolderEvent <$> chooseFolderWidget

update' :: State -> Event -> Transition State Event
update' State (FolderEvent (Right ps)) =
  Transition State (Nothing <$ print ps)
update' State (FolderEvent (Left e)) =
  Transition State (Nothing <$ putStrLn e)
update' _ Closed = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = State
  }
