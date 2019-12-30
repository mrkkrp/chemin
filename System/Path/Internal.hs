{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module System.Path.Internal
  ( Path (..)
  , Platform (..)
  , Base (..)
  , Type (..)
  , Or (..)
  , mk
  , any

  , hostPlatform
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

  , forgetPlatform
  , forgetBase
  , forgetType
  )
where

-- import Control.Applicative
import Control.Monad.Catch (MonadThrow (..))
-- import Data.Foldable (asum)
import Data.Kind (Constraint)
import Data.Proxy
import Prelude hiding (any, abs)
-- import qualified System.Path.Internal.Exception as E
-- import qualified System.Path.Internal.Normalization as N
-- import qualified System.Path.Internal.Predicate as P

data Path (p :: Platform) (b :: Base) (t :: Type) where

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

class Unconstrained (x :: k)
instance Unconstrained x

data ((x :: k) `Or` (y :: k)) (c :: k -> Constraint) where
  Any :: (x `Or` y) Unconstrained
  First :: (x `Or` y) ((~) x)
  Second :: (x `Or` y) ((~) y)

mk ::
  MonadThrow m =>
  ('Posix `Or` 'Win) pc ->
  ('Abs `Or` 'Rel) bc ->
  ('Dir `Or` 'File) tc ->
  FilePath ->
  (forall p b t. (pc p, bc b, tc t) => Path p b t -> m r) ->
  m r
mk Any Any Any path f =
  f (PosixAbsFile path)
mk First First First path f =
  f (PosixAbsDir path)
mk _ _ _ _ _ = undefined

any :: (x `Or` y) Unconstrained
any = Any

#if defined(mingw32_HOST_OS)
hostPlatform :: ('Posix `Or` 'Win) ((~) 'Win)
hostPlatform = Second
#else
hostPlatform :: ('Posix `Or` 'Win) ((~) 'Posix)
hostPlatform = First
#endif

posix :: ('Posix `Or` 'Win) ((~) 'Posix)
posix = First

win :: ('Posix `Or` 'Win) ((~) 'Win)
win = Second

abs :: ('Abs `Or` 'Rel) ((~) 'Abs)
abs = First

rel :: ('Abs `Or` 'Rel) ((~) 'Rel)
rel = Second

dir :: ('Dir `Or` 'File) ((~) 'Dir)
dir = First

file :: ('Dir `Or` 'File) ((~) 'File)
file = Second

pattern IsPosix :: Path 'Posix b t -> Path p b t
pattern IsPosix path <- (isPosix -> Just path)

isPosix :: Path p b t -> Maybe (Path 'Posix b t)
isPosix = \case
  PosixAbsDir path -> Just (PosixAbsDir path)
  PosixAbsFile path -> Just (PosixAbsFile path)
  PosixRelDir path -> Just (PosixRelDir path)
  PosixRelFile path -> Just (PosixRelFile path)
  _ -> Nothing

pattern IsWin :: Path 'Win b t -> Path p b t
pattern IsWin path <- (isWin -> Just path)

isWin :: Path p b t -> Maybe (Path 'Win b t)
isWin = \case
  WinAbsDir path -> Just (WinAbsDir path)
  WinAbsFile path -> Just (WinAbsFile path)
  WinRelDir path -> Just (WinRelDir path)
  WinRelFile path -> Just (WinRelFile path)
  _ -> Nothing

pattern IsAbs :: Path p 'Abs t -> Path p b t
pattern IsAbs path <- (isAbs -> Just path)

isAbs :: Path p b t -> Maybe (Path p 'Abs t)
isAbs = \case
  PosixAbsDir path -> Just (PosixAbsDir path)
  PosixAbsFile path -> Just (PosixAbsFile path)
  WinAbsDir path -> Just (WinAbsDir path)
  WinAbsFile path -> Just (WinAbsFile path)
  _ -> Nothing

pattern IsRel :: Path p 'Rel t -> Path p b t
pattern IsRel path <- (isRel -> Just path)

isRel :: Path p b t -> Maybe (Path p 'Rel t)
isRel = \case
  PosixRelDir path -> Just (PosixRelDir path)
  PosixRelFile path -> Just (PosixRelFile path)
  WinRelDir path -> Just (WinRelDir path)
  WinRelFile path -> Just (WinRelFile path)
  _ -> Nothing

pattern IsDir :: Path p b 'Dir -> Path p b t
pattern IsDir path <- (isDir -> Just path)

isDir :: Path p b t -> Maybe (Path p b 'Dir)
isDir = \case
  PosixAbsDir path -> Just (PosixAbsDir path)
  PosixRelDir path -> Just (PosixRelDir path)
  WinAbsDir path -> Just (WinAbsDir path)
  WinRelDir path -> Just (WinRelDir path)
  _ -> Nothing

pattern IsFile :: Path p b 'File -> Path p b t
pattern IsFile path <- (isFile -> Just path)

isFile :: Path p b t -> Maybe (Path p b 'File)
isFile = \case
  PosixAbsFile path -> Just (PosixAbsFile path)
  PosixRelFile path -> Just (PosixRelFile path)
  WinAbsFile path -> Just (WinAbsFile path)
  WinRelFile path -> Just (WinRelFile path)
  _ -> Nothing

from ::
  Proxy p ->
  Proxy b ->
  Proxy t ->
  Path p b t ->
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

pattern Path :: FilePath -> Path p b t
pattern Path path <- (from Proxy Proxy Proxy -> path)

forgetPlatform :: (forall p1. Path p1 b t -> r) -> Path p0 b t -> r
forgetPlatform f = \case
  PosixAbsDir path -> f (PosixAbsDir path)
  PosixAbsFile path -> f (PosixAbsFile path)
  PosixRelDir path -> f (PosixRelDir path)
  PosixRelFile path -> f (PosixRelFile path)
  WinAbsDir path -> f (WinAbsDir path)
  WinAbsFile path -> f (WinAbsFile path)
  WinRelDir path -> f (WinRelDir path)
  WinRelFile path -> f (WinRelFile path)

forgetBase :: (forall b1. Path p b1 t -> r) -> Path p b0 t -> r
forgetBase f = \case
  PosixAbsDir path -> f (PosixAbsDir path)
  PosixAbsFile path -> f (PosixAbsFile path)
  PosixRelDir path -> f (PosixRelDir path)
  PosixRelFile path -> f (PosixRelFile path)
  WinAbsDir path -> f (WinAbsDir path)
  WinAbsFile path -> f (WinAbsFile path)
  WinRelDir path -> f (WinRelDir path)
  WinRelFile path -> f (WinRelFile path)

forgetType :: (forall t1. Path p b t1 -> r) -> Path p b t0 -> r
forgetType f = \case
  PosixAbsDir path -> f (PosixAbsDir path)
  PosixAbsFile path -> f (PosixAbsFile path)
  PosixRelDir path -> f (PosixRelDir path)
  PosixRelFile path -> f (PosixRelFile path)
  WinAbsDir path -> f (WinAbsDir path)
  WinAbsFile path -> f (WinAbsFile path)
  WinRelDir path -> f (WinRelDir path)
  WinRelFile path -> f (WinRelFile path)
