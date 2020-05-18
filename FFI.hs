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
foreign import javascript unsafe "$1.goToFloor($2)" goToFloor
  :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.on($2, $3)" on
  :: JSVal -> JSString -> Callback (IO ()) -> IO ()
