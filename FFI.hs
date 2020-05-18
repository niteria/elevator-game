module FFI where

import Data.JSString (JSString, pack, unpack)
import Data.Maybe (fromJust)
import GHCJS.Foreign.Callback
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types
import qualified JavaScript.Array as Array
import JavaScript.Array (JSArray)
import JavaScript.Object (Object, create, setProp)

foreign import javascript unsafe "mkCode = $1" setupMkCode
  :: Callback (IO JSVal) -> IO ()

foreign import javascript unsafe "console.log($1)" consoleLog
  :: JSString -> IO ()

-- THE GAME API
-- WARNING: don't forget to update elevator.js.externs when you add
-- something here
-- Elevator
foreign import javascript unsafe "$1.goToFloor($2)" goToFloor
  :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.goToFloor($2, true)" goToFloorNow
  :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.stop()" stop :: JSVal -> IO ()

foreign import javascript unsafe "$1.currentFloor()" currentFloor
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.goingUpIndicator()" getGoingUpIndicator
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.goingUpIndicator($2)" setGoingUpIndicator
  :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.goingDownIndicator()" getGoingDownIndicator
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.goingDownIndicator($2)" setGoingDownIndicator
  :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.maxPassengerCount()" maxPassengerCount
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.loadFactor()" loadFactor
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.destinationDirection()" destinationDirection
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.destinationQueue" getDestinationQueue
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.destinationQueue = $2" setDestinationQueue
  :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.checkDestinationQueue()" checkDestinationQueue
  :: JSVal -> IO ()

foreign import javascript unsafe "$1.getPressedFloors()" getPressedFloors
  :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.on(\"idle\", $2)" onIdle
  :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.on(\"floor_button_pressed\", $2)" onFloorButtonPressed
  :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.on(\"passing_floor\", $2)" onPassingFloor
  :: JSVal -> Callback (JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.on(\"stopped_at_floor\", $2)" onStoppedAtFloor
  :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

-- Floor
foreign import javascript unsafe "$1.floorNum()" floorNum :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.on(\"up_button_pressed\", $2)" onUpButtonPressed
  :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.on(\"down_button_pressed\", $2)" onDownButtonPressed
  :: JSVal -> Callback (IO ()) -> IO ()
