{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ExplicitNamespaces  #-}
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

  , host
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

import Control.Applicative
import Control.Monad.Catch (MonadThrow (..))
import Data.Foldable (asum)
import Data.Proxy
import Prelude hiding (any, abs)
import qualified System.Path.Internal.Exception as E
import qualified System.Path.Internal.Normalization as N
import qualified System.Path.Internal.Predicate as P

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

data ((x :: k) `Or` (y :: k)) (t :: k) where
  Any :: (x `Or` y) t
  First :: (x `Or` y) x
  Second :: (x `Or` y) y

-- foo :: Path p b t
-- foo = PosixAbsDir "/foo/"

-- data SomePath = SomePath (Path p b t)

mk ::
  forall m p b t.
  MonadThrow m =>
  ('Posix `Or` 'Win) p ->
  ('Abs `Or` 'Rel) b ->
  ('Dir `Or` 'File) t ->
  FilePath ->
  m (Path p b t)
mk platform base type' path = undefined -- either throwM return $
  -- case platform of
  --   First ->
  --     N.normalizePosix <$> P.isValidPosix path

  -- either throwM return $ case p of
    -- Any -> checkPlatform First path (checkBase b (checkType t (\_ fp -> _)))

  -- case go p b t path :: Either E.Exception (Path p b t) of
  --   Left e -> throwM e
  --   Right x -> return x
  where

    checkPlatform ::
      ('Posix `Or` 'Win) p' ->
      FilePath ->
      (Platform -> FilePath -> Either E.Exception (Path p' b' t')) ->
      Either E.Exception (Path p' b' t')
    checkPlatform = undefined

    checkBase ::
      ('Abs `Or` 'Rel) b' ->
      Platform ->
      FilePath ->
      (Platform -> FilePath -> Either E.Exception (Path p' b' t')) ->
      Either E.Exception (Path p' b' t')
    checkBase = undefined

    checkType ::
      ('Dir `Or` 'File) t' ->
      Platform ->
      FilePath ->
      (Platform -> FilePath -> Either E.Exception (Path p' b' t')) ->
      Either E.Exception (Path p' b' t')
    checkType = undefined

 -- ::
 --      ('Posix `Or` 'Win) p' ->
 --      ('Abs `Or` 'Rel) b' ->
 --      ('Dir `Or` 'File) t' ->
 --      FilePath ->
 --      Either E.Exception (Path p' b' t')
 --    go Any b t path =
 --      case

-- asum
      -- [ forgetPlatform <$> go First b t path
      -- , forgetPlatform <$> go Second b t path
      -- , Left (E.IsNotValidPath path)
      -- ]

--   case platform of
--     Any ->
--       case host of
--         First ->

--       case P.isValidPosix of
--         Nothing -> case P.isValidWin of
--           Nothing -> throwM (E.IsNotValidAny path)
--           Just path0 ->
--             case base of
--               Any ->

--       if Posix.isValid path
--         then undefined
--         else if Win.isValid path
--           then undefined
--           else

--     First ->
--       if P.isValidPosix path
--         then case base of
--           Any -> if p

-- if P.isValid
--         else throwM (IsNotValidPosix path)
--     Second ->
--       if Win.isValid path
--         then undefined
--         else throwM (IsNotValidWin path)

any :: (x `Or` y) t
any = Any

#if defined(mingw32_HOST_OS)
host :: ('Posix `Or` 'Win) 'Win
host = Second
#else
host :: ('Posix `Or` 'Win) 'Posix
host = First
#endif

posix :: ('Posix `Or` 'Win) 'Posix
posix = First

win :: ('Posix `Or` 'Win) 'Win
win = Second

abs :: ('Abs `Or` 'Rel) 'Abs
abs = First

rel :: ('Abs `Or` 'Rel) 'Rel
rel = Second

dir :: ('Dir `Or` 'File) 'Dir
dir = First

file :: ('Dir `Or` 'File) 'File
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

-- data SomePlatform b t where
--   SomePlatform :: Path p b t -> SomePlatform p t

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

-- TODO So, this is interesting. Come think of it, it should be possible to
-- erase type indices of GADTs because then they are virtually existentials
-- and one would have to pattern match to discover actual types. It's not an
-- invalid thing to do afaiu because it doesn't compromise soundness of the
-- type system in any way. However, to do it we need to either do
-- continuation passing style thingy as shown above in 'forgetPlatform' or
-- to use an existential wrapper.
--
-- The problem with CPS is that we cannot have just a value of that type and
-- put it in e.g. a record.
--
-- The problem with existential wrapper though is that the approach is not
-- composable. We could desire to keep just platform existentially
-- quantified and have the other type parameters concrete and fixed. Or we
-- could pick some other type index such as base (absolute vs relative) and
-- make it existential. But then we could also want to have two of them
-- existential and the other one concrete. This is not nicely expressible in
-- Haskell that we have so far.
