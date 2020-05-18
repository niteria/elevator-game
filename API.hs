{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeApplications
  #-}

module API
  -- Game API
  -- Elevator
  ( goToFloor
  , stop
  , currentFloor
  , getGoingUpIndicator
  , setGoingUpIndicator
  , getGoingDownIndicator
  , setGoingDownIndicator
  , maxPassengerCount
  , loadFactor
  , destinationDirection
  , getDestinationQueue
  , setDestinationQueue
  , checkDestinationQueue
  , getPressedFloors
  , onIdle
  , onFloorButtonPressed
  , onPassingFloor
  , onStoppedAtFloor
  -- Floor
  , floorNum
  , onUpButtonPressed
  , onDownButtonPressed
  -- setup
  , Code(..)
  , InitFunc
  , UpdateFunc
  , setupMkCode
  ) where

import Data.Coerce
import Data.JSString (JSString, pack, unpack)
import Data.Maybe (fromJust)
import qualified FFI
import GHCJS.Foreign.Callback
import GHCJS.Marshal (FromJSVal, fromJSVal, toJSVal)
import GHCJS.Types
import JavaScript.Object (Object, create, setProp)
import Prelude hiding (init)

newtype Elevator =
  Elevator JSVal

newtype Floor =
  Floor JSVal

data Direction
  = Up
  | Down

data DirectionOrStopped
  = Dir Direction
  | Stopped

parseDirOrStoppedStr :: String -> DirectionOrStopped
parseDirOrStoppedStr dirStr =
  case dirStr of
    "up" -> Dir Up
    "down" -> Dir Down
    "stopped" -> Stopped

parseDirStr :: String -> Direction
parseDirStr dirStr =
  case dirStr of
    "up" -> Up
    "down" -> Down

-- Elevator API
-- | Queue the elevator to go to specified floor number.
goToFloor :: Elevator -> Int -> IO ()
goToFloor e f = do
  jsV <- toJSVal f
  FFI.goToFloor (coerce e) jsV

-- | Queue the elevator to go to specified floor number. The elevator will go
-- to that floor directly, and then go to any other queued floors.
goToFloorNow :: Elevator -> Int -> IO ()
goToFloorNow e f = do
  jsV <- toJSVal f
  FFI.goToFloorNow (coerce e) jsV

-- | Clear the destination queue and stop the elevator if it is moving.
-- Note that you normally don't need to stop elevators - it is intended for
-- advanced solutions with in-transit rescheduling logic. Also, note that the
-- elevator will probably not stop at a floor, so passengers will not get out.
stop :: Elevator -> IO ()
stop e = FFI.stop $ coerce e

-- | Gets the floor number that the elevator currently is on.
currentFloor :: Elevator -> IO Int
currentFloor = getter FFI.currentFloor

-- | Gets the going up indicator, which will affect passenger behaviour when
-- stopping at floors.
getGoingUpIndicator :: Elevator -> IO Bool
getGoingUpIndicator = getter FFI.getGoingUpIndicator

-- | Sets the going up indicator, which will affect passenger behaviour when
-- stopping at floors.
setGoingUpIndicator :: Elevator -> Bool -> IO ()
setGoingUpIndicator e v = do
  vJs <- toJSVal v
  FFI.setGoingUpIndicator (coerce e) vJs

-- | Gets the going down indicator, which will affect passenger behaviour when
-- stopping at floors.
getGoingDownIndicator :: Elevator -> IO Bool
getGoingDownIndicator = getter FFI.getGoingDownIndicator

-- | Sets the going down indicator, which will affect passenger behaviour when
-- stopping at floors.
setGoingDownIndicator :: Elevator -> Bool -> IO ()
setGoingDownIndicator e v = do
  vJs <- toJSVal v
  FFI.setGoingDownIndicator (coerce e) vJs

-- | Gets the maximum number of passengers that can occupy the elevator at the
-- same time.
maxPassengerCount :: Elevator -> IO Int
maxPassengerCount = getter FFI.maxPassengerCount

-- | Gets the load factor of the elevator. 0 means empty, 1 means full.
-- Varies with passenger weights, which vary - not an exact measure.
loadFactor :: Elevator -> IO Double
loadFactor = getter FFI.loadFactor

-- | Gets the direction the elevator is currently going to move toward.
destinationDirection :: Elevator -> IO DirectionOrStopped
destinationDirection e = do
  parseDirOrStoppedStr <$> getter FFI.destinationDirection e

-- | The current destination queue, meaning the floor numbers the elevator is
-- scheduled to go to. Can be modified and emptied if desired.
-- Note that you need to call checkDestinationQueue() for the change to take
-- effect immediately.
setDestinationQueue :: Elevator -> [Int] -> IO ()
setDestinationQueue e floorNums = do
  vJs <- toJSVal floorNums
  FFI.setDestinationQueue (coerce e) vJs

-- | The current destination queue, meaning the floor numbers the elevator is
-- scheduled to go to. Can be modified and emptied if desired.
-- Note that you need to call checkDestinationQueue() for the change to take
-- effect immediately.
getDestinationQueue :: Elevator -> IO [Int]
getDestinationQueue = getter FFI.getDestinationQueue

-- | Checks the destination queue for any new destinations to go to. Note that
-- you only need to call this if you modify the destination queue explicitly.
checkDestinationQueue :: Elevator -> IO ()
checkDestinationQueue e = FFI.checkDestinationQueue $ coerce e

-- | Gets the currently pressed floor numbers as an array.
getPressedFloors :: Elevator -> IO [Int]
getPressedFloors = getter FFI.getPressedFloors

-- | Triggered when the elevator has completed all its tasks and is not doing
-- anything.
onIdle :: Elevator -> IO () -> IO ()
onIdle e f = FFI.onIdle (coerce e) =<< asyncCallback f

-- | Triggered when a passenger has pressed a button inside the elevator.
onFloorButtonPressed :: Elevator -> (Int -> IO ()) -> IO ()
onFloorButtonPressed e f =
  FFI.onFloorButtonPressed (coerce e) =<<
  asyncCallback1 (\jsVal -> f . fromJust =<< fromJSVal jsVal)

-- | Triggered slightly before the elevator will pass a floor. A good time to decide whether to stop at that floor. Note that this event is not triggered for the destination floor. Direction is either "up" or "down".
onPassingFloor :: Elevator -> (Int -> Direction -> IO ()) -> IO ()
onPassingFloor e f =
  FFI.onPassingFloor (coerce e) =<<
  asyncCallback2
    (\jsFloor jsDir -> do
       floor <- fromJust <$> fromJSVal jsFloor
       dir <- parseDirStr . fromJust <$> fromJSVal jsDir
       f floor dir)

-- | Triggered when the elevator has arrived at a floor.
onStoppedAtFloor :: Elevator -> (Int -> IO ()) -> IO ()
onStoppedAtFloor e f =
  FFI.onStoppedAtFloor (coerce e) =<<
  asyncCallback1 (\jsVal -> f . fromJust =<< fromJSVal jsVal)

-- Floor API
-- | Gets the floor number of the floor object.
floorNum :: Floor -> IO Int
floorNum = getter FFI.floorNum

-- | Triggered when someone has pressed the up button at a floor. Note that
-- passengers will press the button again if they fail to enter an elevator.
onUpButtonPressed :: Floor -> IO () -> IO ()
onUpButtonPressed floor f =
  FFI.onUpButtonPressed (coerce floor) =<< asyncCallback f

-- | Triggered when someone has pressed the down button at a floor. Note that
-- passengers will press the button again if they fail to enter an elevator.
onDownButtonPressed :: Floor -> IO () -> IO ()
onDownButtonPressed floor f =
  FFI.onDownButtonPressed (coerce floor) =<< asyncCallback f

-- Code object
type InitFunc = [Elevator] -> [Floor] -> IO ()

type UpdateFunc = Double -> [Elevator] -> [Floor] -> IO ()

data Code =
  Code
    { init :: InitFunc
    , update :: UpdateFunc
    }

setupMkCode :: Code -> IO ()
setupMkCode Code {..} = do
  initFFI <-
    asyncCallback2 $ \elevators floors -> do
      elevators' <- coerce . fromJust <$> fromJSVal @[JSVal] elevators
      floors' <- coerce . fromJust <$> fromJSVal @[JSVal] floors
      init elevators' floors'
  updateFFI <-
    asyncCallback3 $ \dt elevators floors -> do
      dt' <- fromJust <$> fromJSVal dt
      elevators' <- coerce . fromJust <$> fromJSVal @[JSVal] elevators
      floors' <- coerce . fromJust <$> fromJSVal @[JSVal] floors
      update dt' elevators' floors'
  mkCode <-
    syncCallback' $ do
      FFI.consoleLog (pack "mkCode")
      (o :: Object) <- create
      setProp (pack "init") (jsval initFFI) o
      setProp (pack "update") (jsval updateFFI) o
      return $ jsval o
  FFI.setupMkCode mkCode

-- Helpers
getter :: (FromJSVal c, Coercible a b) => (b -> IO JSVal) -> a -> IO c
getter f e = do
  r <- f $ coerce e
  fromJust <$> fromJSVal r
