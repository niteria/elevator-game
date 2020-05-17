{-# LANGUAGE ScopedTypeVariables #-}

import Data.JSString (JSString, pack, unpack)
import Data.Maybe (fromJust)
import GHCJS.Foreign.Callback (Callback, asyncCallback1)
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types (JSVal, jsval)
import JavaScript.Object (Object, create, setProp)

foreign import javascript unsafe "onIdle = $1" set_callback
  :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "function() { return console; }()" getConsole
  :: IO JSVal

foreign import javascript unsafe "$1.log($2)" consoleLog
  :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.goToFloor($2)" goToFloor
  :: JSVal -> JSString -> IO ()

main :: IO ()
main = do
  callback <-
    asyncCallback1 $ \elevator -> do
      c <- getConsole
      consoleLog c (pack "dupa")
      goToFloor elevator (pack "0" :: JSString)
      goToFloor elevator (pack "1" :: JSString)
      goToFloor elevator (pack "2" :: JSString)
      goToFloor elevator (pack "0" :: JSString)
      consoleLog c (pack "dupa2")
      return ()
  set_callback callback
