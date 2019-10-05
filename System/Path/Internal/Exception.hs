module System.Path.Internal.Exception
  ( Exception (..)
  )
where

import qualified Control.Exception as E

data Exception
  = IsNotValidPosix FilePath
  | IsNotValidWin FilePath
  deriving (Show, Eq)

instance E.Exception Exception where
  -- displayException = undefined

-- instance Semigroup Exception where
  -- (<>) = And
