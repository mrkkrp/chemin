module System.Path.Internal.Predicate
  ( isValidPosix
  , isValidWin
  )
where

import qualified System.Path.Internal.Exception as E

isValidPosix :: FilePath -> Either E.Exception FilePath
isValidPosix = undefined

isValidWin :: FilePath -> Either E.Exception FilePath
isValidWin = undefined
