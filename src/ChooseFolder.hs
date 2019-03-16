{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module ChooseFolder
  ( soundFileExts
  , chooseFolderWidget
  ) where

import Control.Exception (displayException, try)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust)
import GI.Gtk hiding ((:=), on, main, Widget)
import GI.Gtk.Declarative
import MusicFile
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import qualified Data.Text as T

soundFileExts :: [String]
soundFileExts = [".wav", ".mp3"]

chooseFolderWidget :: Maybe T.Text -> Widget (Either T.Text (Int, [MusicFile]))
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
    Just file | takeExtension file `elem` soundFileExts -> do
      let dir = takeDirectory file
      paths <- keepSoundFiles dir <$> try @IOError (listDirectory dir)
      let i = either (const 0) (fromMaybe 0 . elemIndex file) paths
      either (pure . Left) (fmap (Right . (i, )) . mapM mkMusicFile) paths
    _ -> return $ Left "No music file selected"
   where
    keepSoundFiles _ (Left e) = Left $ T.pack $ displayException e
    keepSoundFiles dir (Right files)
      = Right
      . fmap (dir </>)
      . filter ((`elem` soundFileExts) . takeExtension)
      $ files
