{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module
    HasteCell.World
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import qualified Control.Arrow.Machine as P
import qualified Control.Arrow.Machine.Misc.Discrete as D
import Unsafe.Coerce
import Data.IORef
import HasteCell.ArrowIO
import qualified Haste.Events as Ht
import qualified Haste.DOM as Ht
import Haste.DOM ((=:))



-- IORefのラップ
type MyRef a = IORef a
newMyRef = newIORef
myRefGet = readIORef
myRefSet = writeIORef

-- Event ID
newtype EventID = EventID Int deriving (Eq, Show)

initialID = EventID 0
inclID (EventID n) = EventID (n+1)
newID env =
  do
    let ref = envGetIDPool env
    x <- readIORef ref
    writeIORef ref (inclID x)
    return x

-- Internal data.
data Any

type MainState a = P.ProcessA a
                    (P.Event (EventID, Any)) (P.Event ())

data EventEnv a = EventEnv {
      envGetIDPool :: MyRef EventID,
      envGetState :: MyRef (MainState a),
      envGetRun :: forall b c. a b c -> b -> IO c
    }


data World a = World {
      worldGetEnv :: EventEnv a,
      worldGetEvent :: P.Event (EventID, Any)
}

instance
    P.Occasional' (World a)
  where
    collapse = P.collapse . worldGetEvent


-- Internal functions.
listenID ::
    ArrowApply a =>
    P.ProcessA a (World a, EventID) (P.Event Any)
listenID = proc (World _ etp, myID) ->
  do
    returnA -< P.filterJust $ go myID <$> etp
  where
    go myID (curID, ea) | curID == myID = Just ea
    go _ _ = Nothing

listen ::
    (ArrowIO a, ArrowApply a) =>
    a (Any -> IO ()) h ->
    a Any ev ->
    (h -> IO ()) ->
    P.ProcessA a (World a) (P.Event ev)
listen reg getter disposer = proc (world@(World env _)) ->
  do
    initMsg <- onInit -< world
    evId <- P.anytime (arrIO newID) -< env <$ initMsg

    P.rSwitch (P.muted <<< arr fst) -< ((world, initMsg), listener <$> evId)
  where
    listener myID = proc (world@(World env _), initMsg) ->
      do
        P.anytime reg -< (handleProc env myID) <$ initMsg

        ea <- listenID -< (world, myID)
        P.anytime getter -< ea


handleProc env eid arg =
  do
    stH <- myRefGet $ envGetState env
    (_, stH') <- envGetRun env (P.stepRun stH) (eid, arg)
    envGetState env `myRefSet` stH'


-- |Fires once on initialization.
onInit ::
    (ArrowApply a) =>
    P.ProcessA a (World a) (P.Event ())
onInit = proc world ->
  do
    ea <- listenID -< (world, initialID)
    P.echo -< () <$ ea

--
-- Exports
--
start ::
    (ArrowIO a, ArrowApply a) =>
    (forall b c. a b c -> b -> IO c) ->
    P.ProcessA a (World a) (P.Event ()) ->
    IO ()
start run init =
  do
    rec vID <- newIORef $ inclID initialID
        vSt <- newIORef init'

        let env = EventEnv {
                envGetIDPool = vID,
                envGetState = vSt,
                envGetRun = run
              }
        let init' = proc etp -> init -< World env etp

    handleProc env initialID (unsafeCoerce ())

listenEvent ::
    (Ht.Event evt, ArrowApply a, ArrowIO a) =>
    Ht.ElemID ->
    evt ->
    P.ProcessA a (World a) (P.Event (Ht.EventData evt))
listenEvent elName evt =
    listen
        (arrIO $ \hd ->
          do
             Ht.withElem elName $ \el ->
                 Ht.onEvent el evt (hd . unsafeCoerce)
          )
        (arrIO $ return . unsafeCoerce)
        (Ht.unregisterHandler)

{-
bindProp ::
    (MonadIO m) =>
    Ht.ElemID ->
    Ht.PropID ->
    P.ProcessA (D.T String) ()
bindProp elemId propId =
    arr D.edge >>> P.anytime go >>> pure ()
  where
    go s =
        Ht.withElem elemId $ \el ->
            Ht.setProp el propId s
-}
