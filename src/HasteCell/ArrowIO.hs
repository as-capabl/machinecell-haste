{-# LANGUAGE FlexibleInstances #-}

module
    HasteCell.ArrowIO
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow

class
    ArrowIO a
  where
    arrIO :: (b -> IO c) -> a b c

instance
    ArrowIO (Kleisli IO)
  where
    arrIO = Kleisli

arrIO0 ::
    ArrowIO a => IO c -> a () c
arrIO0 = arrIO . const

arrIO2 ::
    ArrowIO a => (b1 -> b2 -> IO c) -> a (b1, b2) c
arrIO2 = arrIO . uncurry
