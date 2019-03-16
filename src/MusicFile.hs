{-# LANGUAGE OverloadedStrings #-}
module MusicFile
  ( MusicInfo (..)
  , MusicFile (..)
  , albumText
  , albumTitle
  , mkMusicFile
  ) where

import qualified Data.Text as T
import qualified Sound.HTagLib as Tag

data MusicInfo = MusicInfo
  { infoTitle  :: Tag.Title
  , infoArtist :: Tag.Artist
  , infoAlbum  :: Tag.Album
  , infoYear   :: Maybe Tag.Year
  } deriving (Show, Eq)

data MusicFile = MusicFile
  { musicFilePath :: FilePath
  , musicFileInfo :: MusicInfo
  } deriving (Show, Eq)

albumText :: MusicInfo -> T.Text
albumText i = mconcat
  [ Tag.unAlbum (infoAlbum i)
  , " - "
  , Tag.unArtist (infoArtist i)
  , maybe "" (parens . T.pack . show . Tag.unYear) (infoYear i)
  ]
 where parens s = " (" <> s <> ")"

albumTitle :: MusicInfo -> T.Text
albumTitle = Tag.unTitle . infoTitle

musicInfoGetter :: Tag.TagGetter MusicInfo
musicInfoGetter = MusicInfo
  <$> Tag.titleGetter
  <*> Tag.artistGetter
  <*> Tag.albumGetter
  <*> Tag.yearGetter

mkMusicFile :: FilePath -> IO MusicFile
mkMusicFile path = MusicFile <$> pure path <*> Tag.getTags path musicInfoGetter
