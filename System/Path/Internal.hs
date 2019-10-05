{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}

module System.Path.Internal
  ( Path
  , Platform (..)
  , Base (..)
  , Type (..)
  , mk
  , any

  , posix
  , win
  , abs
  , rel
  , dir
  , file

  , pattern IsPosix
  , pattern IsWin
  , pattern IsAbs
  , pattern IsRel
  , pattern IsDir
  , pattern IsFile
  , pattern Path

  , from
  )
where

import Prelude hiding (any, abs)
import qualified Control.Exception as E
import Control.Monad.Catch (MonadThrow)
import Data.Proxy

data Path (s :: Platform) (b :: Base) (t :: Type) where

  PosixAbsDir :: FilePath -> Path 'Posix 'Abs 'Dir
  PosixAbsFile :: FilePath -> Path 'Posix 'Abs 'File
  PosixRelDir :: FilePath -> Path 'Posix 'Rel 'Dir
  PosixRelFile :: FilePath -> Path 'Posix 'Rel 'File

  WinAbsDir :: FilePath -> Path 'Win 'Abs 'Dir
  WinAbsFile :: FilePath -> Path 'Win 'Abs 'File
  WinRelDir :: FilePath -> Path 'Win 'Rel 'Dir
  WinRelFile :: FilePath -> Path 'Win 'Rel 'File

  -- deriving (Eq, Ord) via FilePath

data Platform = Posix | Win

data Base = Abs | Rel

data Type = Dir | File

data Exception = Exception
  deriving (Show, Eq)

instance E.Exception Exception where
  -- displayException = undefined

mk ::
  MonadThrow m =>
  Proxy s ->
  Proxy b ->
  Proxy t ->
  FilePath ->
  m (Path s b t)
mk Proxy Proxy Proxy = undefined

any :: Proxy s
any = Proxy

posix :: Proxy 'Posix
posix = Proxy

win :: Proxy 'Win
win = Proxy

abs :: Proxy 'Abs
abs = Proxy

rel :: Proxy 'Rel
rel = Proxy

dir :: Proxy 'Dir
dir = Proxy

file :: Proxy 'File
file = Proxy

pattern IsPosix :: Path 'Posix b t -> Path s b t
pattern IsPosix path <- (isPosix -> Just path)

isPosix :: Path s b t -> Maybe (Path 'Posix b t)
isPosix = \case
  PosixAbsDir path -> Just (PosixAbsDir path)
  PosixAbsFile path -> Just (PosixAbsFile path)
  PosixRelDir path -> Just (PosixRelDir path)
  PosixRelFile path -> Just (PosixRelFile path)
  _ -> Nothing

pattern IsWin :: Path 'Win b t -> Path s b t
pattern IsWin path <- (isWin -> Just path)

isWin :: Path s b t -> Maybe (Path 'Win b t)
isWin = \case
  WinAbsDir path -> Just (WinAbsDir path)
  WinAbsFile path -> Just (WinAbsFile path)
  WinRelDir path -> Just (WinRelDir path)
  WinRelFile path -> Just (WinRelFile path)
  _ -> Nothing

pattern IsAbs :: Path s 'Abs t -> Path s b t
pattern IsAbs path <- (isAbs -> Just path)

isAbs :: Path s b t -> Maybe (Path s 'Abs t)
isAbs = \case
  PosixAbsDir path -> Just (PosixAbsDir path)
  PosixAbsFile path -> Just (PosixAbsFile path)
  WinAbsDir path -> Just (WinAbsDir path)
  WinAbsFile path -> Just (WinAbsFile path)
  _ -> Nothing

pattern IsRel :: Path s 'Rel t -> Path s b t
pattern IsRel path <- (isRel -> Just path)

isRel :: Path s b t -> Maybe (Path s 'Rel t)
isRel = \case
  PosixRelDir path -> Just (PosixRelDir path)
  PosixRelFile path -> Just (PosixRelFile path)
  WinRelDir path -> Just (WinRelDir path)
  WinRelFile path -> Just (WinRelFile path)
  _ -> Nothing

pattern IsDir :: Path s b 'Dir -> Path s b t
pattern IsDir path <- (isDir -> Just path)

isDir :: Path s b t -> Maybe (Path s b 'Dir)
isDir = \case
  PosixAbsDir path -> Just (PosixAbsDir path)
  PosixRelDir path -> Just (PosixRelDir path)
  WinAbsDir path -> Just (WinAbsDir path)
  WinRelDir path -> Just (WinRelDir path)
  _ -> Nothing

pattern IsFile :: Path s b 'File -> Path s b t
pattern IsFile path <- (isFile -> Just path)

isFile :: Path s b t -> Maybe (Path s b 'File)
isFile = \case
  PosixAbsFile path -> Just (PosixAbsFile path)
  PosixRelFile path -> Just (PosixRelFile path)
  WinAbsFile path -> Just (WinAbsFile path)
  WinRelFile path -> Just (WinRelFile path)
  _ -> Nothing

from ::
  Proxy s ->
  Proxy b ->
  Proxy t ->
  Path s b t ->
  FilePath
from Proxy Proxy Proxy = \case
  PosixAbsDir path -> path
  PosixAbsFile path -> path
  PosixRelDir path -> path
  PosixRelFile path -> path
  WinAbsDir path -> path
  WinAbsFile path -> path
  WinRelDir path -> path
  WinRelFile path -> path

pattern Path :: FilePath -> Path s b t
pattern Path path <- (from Proxy Proxy Proxy -> path)
