{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module
    HasteCell.VDOM
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import HasteCell.ArrowIO

import qualified Control.Arrow.Machine as P
import qualified Control.Arrow.Machine.Misc.Discrete as D
import qualified Haste.Events as Ht
import qualified Haste.DOM as Ht
import Haste.DOM ((=:))

data VDom a b c =
    VDom (Maybe Ht.Elem -> P.ProcessA a b c)

type ElemSig = D.T (Maybe Ht.Elem)

instance
    ArrowApply a => Category (VDom a)
  where
    id = VDom $ const id
    VDom g . VDom f = VDom $ \mel -> g mel . f mel

instance
    ArrowApply a => Arrow (VDom a)
  where
    arr = VDom . const . arr
    first (VDom f) = VDom $ \mel -> first (f mel)

-- Share the parent and return the first element.
infixr 5 <++>
(<++>) ::
    ArrowApply a =>
    VDom a b c -> VDom a b d -> VDom a b ()
VDom f <++> VDom g =
    VDom $ \mel -> f mel &&& g mel >>> pure ()


setToParent Nothing = pure ()
setToParent (Just parent) = proc child ->
  do
    P.anytime (arrIO2 Ht.appendChild) -< (parent,) <$> child
    returnA -< ()

type Tag0 a e = VDom a e ElemSig
tag0 ::
    (ArrowApply a, ArrowIO a) =>
    String -> Tag0 a e
tag0 name = VDom $ \mel -> proc _ ->
  do
    -- create element
    i <- P.now -< ()
    el <- P.anytime (arrIO0 $ Ht.newElem name) -< i

    -- set to parent
    setToParent mel -< el

    --
    D.hold Nothing -< Just <$> el

type Tag1 a e = forall x. VDom a e x -> VDom a e ElemSig
tag1 ::
    (ArrowApply a, ArrowIO a) =>
    String -> Tag1 a e
tag1 name (VDom inner) = VDom $ \mel -> proc env ->
  do
    -- create element
    i <- P.now -< ()
    el <- P.anytime (arrIO0 $ Ht.newElem name) -< i

    -- set to parent
    setToParent mel -< el

    -- activate children
    let inner' x = inner (Just x) >>> pure ()
    P.rSwitch (pure ()) -< (env, inner' <$> el)

    --
    D.hold Nothing -< Just <$> el

text ::
    (ArrowApply a, ArrowIO a) =>
    VDom a (D.T String) (ElemSig)
text = VDom $ \mel -> proc str ->
  do
    -- create element
    i <- P.now -< ()
    let textAtI = D.value str <$ i
    el <- P.anytime (arrIO Ht.newTextElem) -< textAtI

    -- set to parent
    setToParent mel -< el

    -- Update value
    P.rSwitch (D.constant Nothing) -< (str, upd <$> el)
  where
    upd el = proc str ->
      do
        D.asUpdater (arrIO $ Ht.setProp el "nodeValue") -< str
        D.constant $ Just el -< ()

static ::
    (ArrowApply a, ArrowIO a) =>
    String ->
    VDom a e ElemSig
static s = (VDom $ \_ -> D.constant s) >>> text

dom ::
    (ArrowApply a, ArrowIO a) =>
    VDom a b c ->
    P.ProcessA a b c
dom (VDom f) = f Nothing


asChild ::
    (ArrowApply a, ArrowIO a, Ht.IsElem e) =>
    e ->
    P.ProcessA a ElemSig ()
asChild parent = proc ch ->
  do
    cur <- D.edge -< ch
    prev <- P.dHold Nothing -< cur
    P.anytime (arrIO $ go parent) -< (prev,) <$> cur
    returnA -< ()
  where
    go parent (mPrevCh, mCurCh) =
      do
        maybe (return ()) (Ht.deleteChild parent) mPrevCh
        maybe (return ()) (Ht.appendChild parent) mCurCh


div ::
    (ArrowApply a, ArrowIO a) =>
    Tag1 a e
div = tag1 "div"

