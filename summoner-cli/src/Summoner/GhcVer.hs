{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
{- |
Module                  : Summoner.GhcVer
Copyright               : (c) 2017-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Contains data type for GHC versions supported by Summoner
and some useful functions for manipulation with them.
-}

module Summoner.GhcVer
    ( GhcVer
    , GhcMeta (..)
    , Pvp (..)
    , showGhcVer
    , parseGhcVer
    , latestLts
    , ghcVer
    , latestGHCVersion
    , nthGHCVersion
    , allGhcVer
    , baseVer
    , cabalBaseVersions
    , ghcTable
    , oldGhcs
    ) where

import qualified Data.Text as T
import qualified Text.Show as Show
import Data.Version
import Data.Yaml
import Data.FileEmbed
import System.IO.Unsafe
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Text.ParserCombinators.ReadP (readP_to_S, eof)
import Control.Monad.Catch
import System.Environment.XDG.BaseDir
import System.FilePath
import System.IO.Error
import qualified Data.ByteString as BS

-- | Represents some selected set of GHC versions.
data GhcVer
    = GhcVer { ghcVer :: !Version -- ^ Ghc version
             , latestLts :: !Text -- ^ Returns latest known LTS resolver for all GHC versions except default one.
             , baseVerPvp :: !Pvp
             , isOld :: !Bool
             } deriving stock (Show)

instance Eq GhcVer where
  (==) = (==) `on` ghcVer
  (/=) = (/=) `on` ghcVer

instance Ord GhcVer where
  compare = compare `on` ghcVer

newtype Config = Config { unConfig :: M.Map Version GhcVer }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config . M.fromList <$>
      forM (HM.toList o) (\(k, v) -> do
        k' <- parseJSON (String k)
        v' <- parseGV k' v
        pure (k', v'))
    where
    parseGV ver = withObject "GhcVer" $ \v -> GhcVer ver
      <$> v .: "resolver"
      <*> v .: "base"
      <*> v .:? "old" .!= False

instance FromJSON Pvp where
  parseJSON v = do
    version <- parseJSON v
    case version of
      Version{versionBranch=[a,b,c,d]} -> pure $ Pvp a b c d
      _ -> fail "Invalid base version"

{-# NOINLINE remoteConfig #-}
remoteConfig :: Maybe ByteString
remoteConfig = unsafePerformIO $ do
    cacheDir <- getUserCacheDir "summoner"
    Just <$> BS.readFile (cacheDir </> "config.yaml")
  `catch` \h -> let _ = (h :: IOError) in
    return Nothing

localConfig :: ByteString
localConfig = $(embedFile "config.yaml")

config :: Config
config = case decodeEither' <$> remoteConfig of
    Just (Right a) -> a
    _ -> case decodeEither' localConfig of
      Right a -> a
      Left err -> error . T.pack $ show err

latestGHCVersion :: GhcVer
latestGHCVersion = snd . M.findMax $ unConfig config

nthGHCVersion :: Int -> GhcVer
nthGHCVersion n = case snd <$> reverse (M.toList . unConfig $ config) !!? n of
  Just r -> r
  Nothing -> error $ "Failed to get " <> show n <> "th GHC version"

-- | Converts 'GhcVer' into dot-separated string.
showGhcVer :: GhcVer -> Text
showGhcVer = T.pack . showVersion . ghcVer

allGhcVer :: [GhcVer]
allGhcVer = M.elems $ unConfig config

{- | These are old GHC versions that are not working with default GHC versions
when using Stack.
-}
oldGhcs :: [GhcVer]
oldGhcs = filter isOld allGhcVer

parseGhcVer :: Text -> Maybe GhcVer
parseGhcVer t =
  case readP_to_S (parseVersion <* eof) (T.unpack t) of
  [(v, "")] -> M.lookup v $ unConfig config
  _ -> Nothing

-- | Represents PVP versioning (4 numbers).
data Pvp = Pvp
    { pvpFirst  :: !Int
    , pvpSecond :: !Int
    , pvpThird  :: !Int
    , pvpFourth :: !Int
    }

-- | Show PVP version in a standard way: @1.2.3.4@
instance Show Pvp where
    show (Pvp a b c d) = intercalate "." $ map Show.show [a, b, c, d]

-- | Returns corresponding @base@ version of the given GHC version.
baseVer :: GhcVer -> Text
baseVer = show . baseVerPvp

{- | Returns the @base@ bounds for the list of the given GHC versions.

>>> cabalBaseVersions [Ghc844]
"^>= 4.11.1.0"

>>> cabalBaseVersions [Ghc802, Ghc822, Ghc844]
">= 4.9.0.0 && < 4.12"

-}
cabalBaseVersions :: [GhcVer] -> Text
cabalBaseVersions ghcVers = case sort ghcVers of
    [] -> ""
    [v] -> "^>= " <> baseVer v
    minGhc:x:xs -> ">= " <> baseVer minGhc <> " && < " <> upperBound (x :| xs)
  where
    upperBound :: NonEmpty GhcVer -> Text
    upperBound ghcs = let Pvp{..} = baseVerPvp $ last ghcs in
        show pvpFirst <> "." <> show (pvpSecond + 1)

-- | Data type to keep meta information for every 'GhcVer'.
data GhcMeta = GhcMeta
    { gmGhc      :: !Text
    , gmBase     :: !Text
    , gmResolver :: !Text
    }

-- | Create corresponding 'GhcMeta' from the given 'GhcVer'.
toGhcMeta :: GhcVer -> GhcMeta
toGhcMeta ghcVer = GhcMeta
    { gmGhc      = "GHC-" <> showGhcVer ghcVer
    , gmBase     = "base-" <> baseVer ghcVer
    , gmResolver = latestLts ghcVer
    }

ghcTable :: [Text]
ghcTable = map (formatGhcMeta . toGhcMeta) allGhcVer

{- Formats 'GhcMeta' in a special way.
It aligns the meta to the left, filling on the right with the spaces.

As the pad number it takes the maximum possible length of the data manually.

Example:

@
GHC-8.6.5     base-4.12.0.0   lts-14.17
@
-}
formatGhcMeta :: GhcMeta -> Text
formatGhcMeta GhcMeta{..} =
       T.justifyLeft 12 ' ' gmGhc
    <> "  "
    <> T.justifyLeft 14 ' ' gmBase
    <> "  "
    <> gmResolver
