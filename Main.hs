{-# LANGUAGE ScopedTypeVariables #-}

import API
import Data.JSString (JSString, pack, unpack)
import Data.Maybe (fromJust)
import qualified FFI
import GHCJS.Foreign.Callback
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types
import qualified JavaScript.Array as Array
import JavaScript.Array (JSArray)
import JavaScript.Object (Object, create, setProp)
import Prelude hiding (init)

exampleInit :: InitFunc
exampleInit (elevator:_) _ = do
  on elevator "idle" $ do
    goToFloor elevator 0
    goToFloor elevator 1
    goToFloor elevator 2
    goToFloor elevator 3

exampleCode :: Code
exampleCode = Code {init = exampleInit, update = \_ _ _ -> return ()}

main :: IO ()
main = setupMkCode exampleCode
