{-# LANGUAGE ScopedTypeVariables #-}

import API
import Control.Concurrent
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

exampleInit :: MVar Int -> InitFunc
exampleInit mvar (elevator:_) _ = do
  on elevator "idle" $ do
    count <- modifyMVar mvar (return . (\a -> (a, a)) . (+ 1))
    goToFloor elevator 0
    goToFloor elevator 1
    goToFloor elevator 2
    goToFloor elevator 3

exampleUpdate :: MVar Int -> UpdateFunc
exampleUpdate mvar _ _ _ = do
  count <- modifyMVar mvar (return . (\a -> (a, a)) . (+ 1))
  return ()

exampleCode :: IO Code
exampleCode = do
  mvar <- newMVar 0
  return Code {init = exampleInit mvar, update = exampleUpdate mvar}

main :: IO ()
main = setupMkCode =<< exampleCode
