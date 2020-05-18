{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeApplications
  #-}

module API
  -- Game API
  ( goToFloor
  , on
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
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types
import JavaScript.Object (Object, create, setProp)
import Prelude hiding (init)

newtype Elevator =
  Elevator JSVal

newtype Floor =
  Floor JSVal

on :: Elevator -> String -> IO () -> IO ()
on e event f = FFI.on (coerce e) (pack event) =<< asyncCallback f

goToFloor :: Elevator -> Int -> IO ()
goToFloor e f = FFI.goToFloor (coerce e) (pack $ show f)

type InitFunc = [Elevator] -> [Floor] -> IO ()

type UpdateFunc = Int -> [Elevator] -> [Floor] -> IO () -- TODO: is it Int or Double?

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
