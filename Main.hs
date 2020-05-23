{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

import API
import Control.Concurrent
import Control.Monad
import Control.Monad.Extra
import Data.JSString (JSString, pack, unpack)
import Data.List hiding (init)
import Data.List.Extra hiding (init)
import Data.Maybe (fromJust)
import qualified FFI
import GHCJS.Foreign.Callback
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types
import qualified JavaScript.Array as Array
import JavaScript.Array (JSArray)
import JavaScript.Object (Object, create, setProp)
import Prelude hiding (init)
import qualified Prelude

elevatorLeastLoaded :: [Elevator] -> IO (IO Elevator)
elevatorLeastLoaded elevators =
  return $ do
    snd . head . sortOn fst <$> mapM (\a -> (, a) <$> loadFactor a) elevators

elevatorMostLoaded :: [Elevator] -> IO (IO Elevator)
elevatorMostLoaded elevators =
  return $ do
    snd . head . sortOn fst <$> mapM (\a -> (, a) <$> loadFactor a) elevators

elevatorRR :: [Elevator] -> IO (IO Elevator)
elevatorRR elevators = do
  let n = length elevators
  rrIdx <- newMVar 0
  return $ do
    modifyMVar rrIdx $ \i -> do return ((i + 1) `mod` n, elevators !! i)

elevatorClosestNotFull :: [Elevator] -> IO (Int -> IO Elevator)
elevatorClosestNotFull elevators = do
  return $ \floor -> do
    evs <-
      forM elevators $ \elevator -> do
        lf <- loadFactor elevator
        cf <- currentFloor elevator
        return (lf < 0.7, abs (cf - floor), elevator)
    return $ (\(_, _, c) -> c) . head $ sortOn (\(a, b, c) -> (a, b)) evs

exampleInit :: InitFunc
exampleInit elevators floors = do
  getNextElevator <- elevatorLeastLoaded elevators
  floorNums <- mapM floorNum floors
  forM_ elevators $ \elevator -> do
    onFloorButtonPressed elevator $ \floorN -> do
      lf <- loadFactor elevator
      if lf > 0.5
        then do
          pressedFloors <- getPressedFloors elevator
          let minPressed = minimum pressedFloors
              maxPressed = maximum pressedFloors
          currentF <- currentFloor elevator
          let destQ =
                if currentF - minPressed > maxPressed - currentF
                  then reverse $ sort pressedFloors
                  else sort pressedFloors
          setDestinationQueue elevator destQ
          checkDestinationQueue elevator
        else goToFloor elevator floorN
    onIdle elevator $ do goToFloor elevator 0
  forM_ floors $ \floor -> do
    let onFloorButton = do
          elevator <- getNextElevator
          goToFloor elevator =<< floorNum floor
    onUpButtonPressed floor onFloorButton
    onDownButtonPressed floor onFloorButton

init4 :: InitFunc
init4 elevators floors = do
  floorNums <- mapM floorNum floors
  let firstFloorNum = minimum floorNums
  let lastFloorNum = maximum floorNums
  forM_ elevators $ \elevator -> do
    onIdle elevator $ do
      goToFloor elevator lastFloorNum
      goToFloor elevator firstFloorNum
    onFloorButtonPressed elevator $ \floorNum -> goToFloor elevator floorNum

init5 :: InitFunc
init5 elevators floors = do
  getNextElevator <- elevatorLeastLoaded elevators
  floorNums <- mapM floorNum floors
  forM_ elevators $ \elevator -> do
    onFloorButtonPressed elevator $ \floorN -> do
      lf <- loadFactor elevator
      if lf > 0.5
        then do
          pressedFloors <- getPressedFloors elevator
          let minPressed = minimum pressedFloors
              maxPressed = maximum pressedFloors
          currentF <- currentFloor elevator
          let destQ =
                if currentF - minPressed > maxPressed - currentF
                  then reverse $ sort pressedFloors
                  else sort pressedFloors
          setDestinationQueue elevator destQ
          checkDestinationQueue elevator
        else goToFloor elevator floorN
    onIdle elevator $ do goToFloor elevator 0
  forM_ floors $ \floor -> do
    let onFloorButton = do
          elevator <- getNextElevator
          goToFloor elevator =<< floorNum floor
    onUpButtonPressed floor onFloorButton
    onDownButtonPressed floor onFloorButton

init6 :: InitFunc
init6 elevators floors = do
  getNextElevator <- elevatorLeastLoaded elevators
  floorNums <- mapM floorNum floors
  forM_ elevators $ \elevator -> do
    onFloorButtonPressed elevator $ \floorN -> do
      lf <- loadFactor elevator
      if lf > 0.9
        then do
          pressedFloors <- getPressedFloors elevator
          let minPressed = minimum pressedFloors
              maxPressed = maximum pressedFloors
          currentF <- currentFloor elevator
          let destQ =
                if currentF - minPressed > maxPressed - currentF
                  then reverse $ sort pressedFloors
                  else sort pressedFloors
          setDestinationQueue elevator destQ
          checkDestinationQueue elevator
        else return ()
  forM_ floors $ \floor -> do
    let onFloorButton = do
          elevator <- getNextElevator
          goToFloor elevator =<< floorNum floor
    onUpButtonPressed floor onFloorButton
    onDownButtonPressed floor onFloorButton

init7 :: InitFunc
init7 elevators floors = do
  getNextElevator <- elevatorMostLoaded elevators
  floorNums <- mapM floorNum floors
  forM_ elevators $ \elevator -> do
    let startDropping = do
          lf <- loadFactor elevator
          FFI.consoleLog $ pack ("lf " ++ show lf)
          if lf > lfThreshold
            then do
              pressedFloors <- getPressedFloors elevator
              let minPressed = minimum pressedFloors
                  maxPressed = maximum pressedFloors
              currentF <- currentFloor elevator
              let destQ =
                    if currentF - minPressed > maxPressed - currentF
                      then reverse $ sort pressedFloors
                      else sort pressedFloors
              FFI.consoleLog
                (pack
                   ("startDropping " ++
                    show pressedFloors ++
                    ", destQ " ++ show destQ ++ ", currentF " ++ show currentF))
              setDestinationQueue elevator destQ
              checkDestinationQueue elevator
            else return ()
    onFloorButtonPressed elevator $ \floorN -> startDropping
    onIdle elevator startDropping
  forM_ floors $ \floor -> do
    let onFloorButton = do
          elevator <- getNextElevator
          lf <- loadFactor elevator
          when (lf <= lfThreshold) $ do goToFloor elevator =<< floorNum floor
    onUpButtonPressed floor onFloorButton
    onDownButtonPressed floor onFloorButton
  where
    lfThreshold = 0.7

init8 :: InitFunc
init8 elevators floors = do
  getNextElevator <- elevatorLeastLoaded elevators
  floorNums <- mapM floorNum floors
  forM_ elevators $ \elevator -> do
    let startDropping = do
          lf <- loadFactor elevator
          FFI.consoleLog $ pack ("lf " ++ show lf)
          if lf > lfThreshold
            then do
              pressedFloors <- getPressedFloors elevator
              let minPressed = minimum pressedFloors
                  maxPressed = maximum pressedFloors
              currentF <- currentFloor elevator
              let destQ =
                    if currentF - minPressed > maxPressed - currentF
                      then reverse $ sort pressedFloors
                      else sort pressedFloors
              FFI.consoleLog
                (pack
                   ("startDropping " ++
                    show pressedFloors ++
                    ", destQ " ++ show destQ ++ ", currentF " ++ show currentF))
              setDestinationQueue elevator destQ
              checkDestinationQueue elevator
            else return ()
    return ()
    onFloorButtonPressed elevator $ \floorN -> goToFloor elevator floorN
    -- onIdle elevator startDropping
  forM_ floors $ \floor -> do
    let onFloorButton = do
          elevator <- getNextElevator
          lf <- loadFactor elevator
          goToFloor elevator =<< floorNum floor
    onUpButtonPressed floor onFloorButton
    onDownButtonPressed floor onFloorButton
  where
    lfThreshold = 0.0

init10 :: InitFunc
init10 elevators floors = do
  getNextElevator <- elevatorLeastLoaded elevators
  floorNums <- mapM floorNum floors
  forM_ elevators $ \elevator -> do
    let startDropping = do
          lf <- loadFactor elevator
          FFI.consoleLog $ pack ("lf " ++ show lf)
          if lf > lfThreshold
            then do
              pressedFloors <- getPressedFloors elevator
              let minPressed = minimum pressedFloors
                  maxPressed = maximum pressedFloors
              currentF <- currentFloor elevator
              let destQ =
                    if currentF - minPressed > maxPressed - currentF
                      then reverse $ sort pressedFloors
                      else sort pressedFloors
              FFI.consoleLog
                (pack
                   ("startDropping " ++
                    show pressedFloors ++
                    ", destQ " ++ show destQ ++ ", currentF " ++ show currentF))
              setDestinationQueue elevator destQ
              checkDestinationQueue elevator
            else return ()
    onFloorButtonPressed elevator $ \floorN -> startDropping
    onIdle elevator startDropping
  forM_ floors $ \floor -> do
    let onFloorButton = do
          elevator <- getNextElevator
          goToFloor elevator =<< floorNum floor
    onUpButtonPressed floor onFloorButton
    onDownButtonPressed floor onFloorButton
  where
    lfThreshold = 0.4

init11 :: InitFunc
init11 elevators floors = do
  let ne = length elevators
  forM_ (zip [0 ..] elevators) $ \(eid, e) -> do
    onIdle e $ goToFloor e 0
    onFloorButtonPressed e (goToFloor e)
    forM_ (zip [1 ..] $ tail floors) $ \(fid, f) -> do
      when (fid `mod` ne == eid) $ do onDownButtonPressed f $ goToFloor e fid
    onPassingFloor e $ \floorN direction -> do
      pressedFloors <- getPressedFloors e
      let exists = floorN `elem` pressedFloors
      when exists $ do
        goToFloor e floorN
        destQ <- getDestinationQueue e
        let destQ' = last destQ : (Prelude.init destQ ++ [head destQ])
        setDestinationQueue e destQ'
        checkDestinationQueue e

efoutsInit :: InitFunc
efoutsInit elevators floors = do
  forM_ elevators $ \e -> do
    onFloorButtonPressed e $ \f -> do
      isD <- isDestination e f
      unless isD $ goToFloor e f
    onPassingFloor e $ \f _ -> do
      isD <- isDestination e f
      when isD $ do
        destQ <- filter (/= f) <$> getDestinationQueue e
        setDestinationQueue e destQ
        goToFloorNow e f
  forM_ floors $ \f -> do
    let onFloorButton = do
          fNum <- floorNum f
          alreadyPlanned <- anyM (\e -> isDestination e fNum) elevators
          unless alreadyPlanned $ do
            shortestQE <-
              snd . minimumOn fst <$>
              forM elevators (\e -> (, e) . length <$> getDestinationQueue e)
            isD <- isDestination shortestQE fNum
            unless isD $ goToFloor shortestQE fNum
    onUpButtonPressed f onFloorButton
    onDownButtonPressed f onFloorButton
  where
    isDestination :: Elevator -> Int -> IO Bool
    isDestination e f = (f `elem`) <$> getDestinationQueue e

exampleUpdate :: UpdateFunc
exampleUpdate _ _ _ = do
  return ()

exampleCode :: IO Code
exampleCode = do
  return Code {init = efoutsInit, update = exampleUpdate}

main :: IO ()
main = setupMkCode =<< exampleCode
