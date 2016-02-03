{-# LANGUAGE Arrows #-}

import Prelude hiding (id, (.), div)
import Control.Category
import Control.Arrow
import Data.Functor ((<$))

import qualified Haste.Events as Ht
import qualified Haste.DOM as Ht

import Control.Arrow.Machine.Types
import qualified Control.Arrow.Machine as Mc
import qualified Control.Arrow.Machine.Misc.Discrete as D
import HasteCell.VDOM
import HasteCell.World
import HasteCell.ArrowIO

mainArrow ::
    Ht.Elem ->
    ProcessA (Kleisli IO) (World (Kleisli IO)) (Event ())
mainArrow root = proc world ->
  do
    click <- listenEvent "btn" Ht.Click -< world
    s2 <- D.accum "hoge" -< ("+" ++) <$ click

    node <- (|dom
      (
        (|div
          (
            (|div (static "Hello!"-<()) |) <++>
            (|div (text -< s2)|)
          )|)
      )|)
    asChild root -< node

    Mc.muted -< world

main =
  do
    Ht.withElem "a" $ \root ->
        start runKleisli (mainArrow root)
