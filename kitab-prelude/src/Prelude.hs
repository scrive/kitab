{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
  ( module Control.Applicative
  , module Control.Monad
  , module Data.Either
  , module Data.Foldable
  , module Data.Text.Display
  , module Data.Functor
  , module Data.Maybe
  , module P
  , module Data.Ord
  , module Data.Eq
  , module Data.Monoid
  , module Control.Monad.IO.Class
  , List
  , Text
  , Vector
  , identity
  , (&)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import Data.Eq
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Data.List (List)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Text.Show

import "base" Prelude hiding (id, unzip)
import "base" Prelude qualified as P

identity :: a -> a
identity a = a
