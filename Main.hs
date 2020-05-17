{-# LANGUAGE ScopedTypeVariables #-}

import Data.JSString (JSString, pack, unpack)
import Data.Maybe (fromJust)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types (JSVal, jsval)
import JavaScript.Object (Object, create, setProp)

foreign import javascript unsafe "helloWorld = $1" set_callback
  :: Callback (JSVal -> IO JSVal) -> IO ()

main :: IO ()
main = do
  callback <-
    syncCallback1' $ \jv -> do
      (str :: String) <- unpack . fromJust <$> fromJSVal jv
      (o :: Object) <- create
      setProp
        (pack "helloworld" :: JSString)
        (jsval . pack $ "hello, " ++ str)
        o
      return $ jsval o
  set_callback callback
