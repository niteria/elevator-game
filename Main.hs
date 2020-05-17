{-# LANGUAGE ScopedTypeVariables #-}

import Data.JSString (JSString, pack, unpack)
import Data.Maybe (fromJust)
import GHCJS.Foreign.Callback
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types
import qualified JavaScript.Array as Array
import JavaScript.Array (JSArray)
import JavaScript.Object (Object, create, setProp)

foreign import javascript unsafe "init = $1" setupInit
  :: Callback (JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "update = $1" setupUpdate
  :: Callback (JSVal -> JSVal -> JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "function() { return console; }()" getConsole
  :: IO JSVal

foreign import javascript unsafe "$1.log($2)" consoleLog
  :: JSVal -> JSString -> IO ()

-- XXX: don't forget to update elevator.js.externs
foreign import javascript unsafe "$1.goToFloor($2)" goToFloor
  :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.on($2, $3)" on
  :: JSVal -> JSString -> Callback (IO ()) -> IO ()

main :: IO ()
main = do
  setupInit =<<
    asyncCallback2
      (\elevators floors -> do
         Just (elevator:_) <- fromJSVal elevators
         on elevator (pack "idle") =<<
           asyncCallback
             (do goToFloor elevator (pack "0" :: JSString)
                 goToFloor elevator (pack "1" :: JSString)
                 goToFloor elevator (pack "2" :: JSString)
                 goToFloor elevator (pack "0" :: JSString)
                 return ())
         return ())
  setupUpdate =<<
    asyncCallback3
      (\dt elevators floors -> do
         c <- getConsole
         -- consoleLog c (pack "update")
         return ())
