{-# LANGUAGE OverloadedStrings #-}

module SDLEventProvider.SDLKeyEventProvider where

import Network.MateLight.Simple
import Network.MateLight

import qualified SDL
import SDL.Input.Keyboard as SDLKeys
import SDL.Input.Joystick as SDLJoystick
import SDL.Input.Keyboard.Codes as SDLKeyCodes

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Char
import Data.List
import Data.Foldable
import qualified Data.Vector

data KeyState = KeyState {
     pressed :: [(String, Integer)]
    ,held :: [(String, Integer)]
    ,released :: [(String, Integer)]
    } deriving (Read, Show)

data InternalKeyState = InternalKeyState {
     pressedI :: [(String, Integer)]
    ,heldI :: [(String, Integer, Integer)]
    ,releasedI :: [(String, Integer)]
    } deriving (Read, Show)

data ButtonState = ButtonState {
     pressedB :: [(String, Integer)]
    ,heldB :: [(String, Integer)]
    ,releasedB :: [(String, Integer)]
    } deriving (Read, Show)
    
data InternalButtonState = InternalButtonState {
     pressedBI :: [(String, Integer)]
    ,heldBI :: [(String, Integer, Integer)]
    ,releasedBI :: [(String, Integer)]
    } deriving (Read, Show)

-----------------------------------------------------

showSDLControlWindow :: IO SDL.Window
showSDLControlWindow = do 
    SDL.initializeAll
    window <- SDL.createWindow "FritzLight SDL Control Area" $ SDL.defaultWindow {SDL.windowInitialSize = (SDL.V2 300 100)}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
    SDL.clear renderer
    threadDelay 10000 -- Necessary for the renderer to be completely cleared?
    SDL.present renderer
    return window

destroySDLControlWindow :: SDL.Window -> IO ()
destroySDLControlWindow = SDL.destroyWindow

sdlJoystickEventProvider :: TChan EventT -> IO ()
sdlJoystickEventProvider channel = do 
    startTime <- getCurrentTime
    joystickCount <- SDLJoystick.numJoysticks
    mPutStrLn $ (show $ joystickCount) ++ " joystick(s) found."
    when (joystickCount > 0) $ do
        joystickDevices <- SDLJoystick.availableJoysticks
        joysticks <- mapM (\jStickNum -> SDLJoystick.openJoystick ((Data.Vector.!) joystickDevices jStickNum)) [0..(length joystickDevices)-1]
        forM_ [0..(length joystickDevices)-1] $ \n -> do
            let joystick = joysticks!!n
            buttonCount <- SDLJoystick.numButtons joystick
            mPutStrLn $ "Joystick " ++ (show n) ++ " has " ++ (show $ buttonCount) ++ " buttons."
            axisCount <- SDLJoystick.numAxes joystick
            mPutStrLn $ "Joystick " ++ (show n) ++ " has " ++ (show $ axisCount) ++ " axes."
            ballCount <- SDLJoystick.numBalls joystick
            mPutStrLn $ "Joystick " ++ (show n) ++ " has " ++ (show $ ballCount) ++ " balls."
        sdlJoystickEventGen channel (InternalButtonState [] [] []) startTime joysticks

sdlGetJoystickButtonData :: Joystick -> Int -> IO [(String,Bool)]
sdlGetJoystickButtonData joystick id = do
    buttonCount <- SDLJoystick.numButtons joystick
    pressedButtonBools <- mapM (\buttonNum -> SDLJoystick.buttonPressed joystick buttonNum) $ [0..buttonCount-1]
    axisCount <- SDLJoystick.numAxes joystick
    axisValues <- mapM (\axisNum -> SDLJoystick.axisPosition joystick axisNum) $ [0..axisCount-1]
    pressedAxisBools <- foldM (\acc num -> return (acc ++ [(axisValues!!num) < 0, (axisValues!!num) > 0])) [] [0..(length axisValues)-1]
    let zippedButtonBools = zip (map (\b -> "P" ++ (show id) ++ "_Button_" ++ (show b)) [0..buttonCount-1]) pressedButtonBools
        zippedAxisBools = zip (map (\(ax, dir) -> "P" ++ (show id) ++ "_Axis_" ++ (show ax) ++ "_D" ++ (show dir)) (zip (concatMap (replicate 2) [0..axisCount-1]) (concat $ replicate ((length pressedAxisBools) `div` 2) [0,1]))) pressedAxisBools     
    let buttonsBase = ["A", "B", "X", "Y", "L", "R", "SELECT", "START", "UP", "DOWN", "LEFT", "RIGHT"]
    return $ (zippedButtonBools ++ zippedAxisBools)
    
sdlJoystickEventGen :: TChan EventT -> InternalButtonState -> UTCTime -> [Joystick] -> IO ()
sdlJoystickEventGen channel internButtonState startTime joysticks = do
    time <- getCurrentTime
    let timeMillis = round (realToFrac ((diffUTCTime time startTime) * 1000) :: Float) :: Integer
    zippedButtonBoolsLists <- (mapM (\(joystick,num) -> sdlGetJoystickButtonData joystick num) $ zip joysticks [0..])
    zippedButtonBools <- foldM ((\acc item -> return (acc ++ item))) [] zippedButtonBoolsLists
    let newInternButtonState = InternalButtonState {pressedBI=pK, heldBI=hK, releasedBI=rK} 
                                where (pK2, rK2) = foldr (\key (accP, accR) -> case key of {(k,True) -> ((k,timeMillis) : accP, accR); 
                                                                                            (k,False) -> (accP, (k,timeMillis) : accR)}) ([],[]) zippedButtonBools
                                      pKolds = map (\(key,time) -> key) (pressedBI internButtonState)
                                      rKolds = map (\(key,time) -> key) (releasedBI internButtonState)
                                      hKolds = map (\(key,time,dur) -> key) (heldBI internButtonState)
                                      rK = filter (\(key,start) -> (key `elem` hKolds) || (key `elem` pKolds)) rK2
                                      rKs = map (\(key,time) -> key) rK
                                      hK = nub $ (map (\(key,start,dur) -> (key,start,(timeMillis-start))) (filter (\(key,start,dur) -> not $ key `elem` rKs) (heldBI internButtonState))) ++ 
                                                 (map (\(key,tStart) -> (key, timeMillis, 0)) (filter (\(key,tStart) -> not $ key `elem` (rKolds++rKs)) (pressedBI internButtonState)))
                                      hKs = map (\(key,time,dur) -> key) hK
                                      pK = filter (\(key,tStart) -> not $ key `elem` (hKolds++hKs)) pK2
        newButtonState = ButtonState {pressedB=(pressedBI newInternButtonState), releasedB=(releasedBI newInternButtonState), heldB=(map (\(key,start,dur) -> (key,dur)) (heldBI newInternButtonState))}
        quit = all (\button -> button `elem` (map (\(b,_) -> b) (pressedB newButtonState))) ["A","B","L","R"]
    
    atomically $ writeTChan channel $ EventT "SDL_JOYSTICK_DATA" newButtonState
    threadDelay 33000
    unless quit (sdlJoystickEventGen channel newInternButtonState startTime joysticks)
    
    
sdlKeyEventProvider :: TChan EventT -> IO ()
sdlKeyEventProvider channel = do 
    startTime <- getCurrentTime
    sdlKeyEventGen channel (InternalKeyState [] [] []) startTime
    
sdlKeyEventGen :: TChan EventT -> InternalKeyState -> UTCTime -> IO ()
sdlKeyEventGen channel internKeyState startTime = do
    time <- getCurrentTime
    let timeMillis = round (realToFrac ((diffUTCTime time startTime) * 1000) :: Float) :: Integer
    events <- SDL.pollEvents
    keyMap <- SDLKeys.getKeyboardState
    let eventKey event =
            case SDL.eventPayload event of
                SDL.KeyboardEvent keyboardEvent ->
                    let evType = SDL.keyboardEventKeyMotion keyboardEvent
                        pressed = evType == SDL.Pressed
                        released = evType == SDL.Released in
                    if pressed || released then
                        let scanCode = SDL.keysymScancode $ SDL.keyboardEventKeysym keyboardEvent
                            keyCode = SDL.keysymKeycode $ SDL.keyboardEventKeysym keyboardEvent                            
                            keyNum = fromIntegral $ unwrapKeycode $ keyCode
                            key | (keyNum >= 0x61 && keyNum <= 0x7A) = [chr $ 0x41 + (keyNum-0x61)]
                                | (keyNum >= 0x30 && keyNum <= 0x39) = show(keyNum-48)
                                | (keyNum >= 0x4000003A && keyNum <= 0x40000045) = "F" ++ (show (keyNum-0x4000003A+1))
                                | keyCode == SDL.KeycodeLCtrl = "L-CTRL"
                                | keyCode == SDL.KeycodeRCtrl = "R-CTRL"
                                | keyCode == SDL.KeycodeLShift = "L-SHIFT"
                                | keyCode == SDL.KeycodeRShift = "R-SHIFT"
                                | keyCode == SDL.KeycodeCapsLock = "CAPS_LOCK"
                                | keyCode == SDL.KeycodeTab = "TAB"
                                | keyCode == SDL.KeycodeBackspace = "BACKSPACE"
                                | keyCode == SDL.KeycodeLGUI = "L-GUI"
                                | keyCode == SDL.KeycodeRGUI = "R-GUI"
                                | keyCode == SDL.KeycodeMode = "MODE"
                                | keyCode == SDL.KeycodeLAlt = "L-ALT"
                                | keyCode == SDL.KeycodeRAlt = "R-ALT"
                                | keyCode == SDL.KeycodeReturn = "RETURN"
                                | keyCode == SDL.KeycodeSpace = "SPACE"
                                | keyCode == SDL.KeycodeEscape = "ESC"
                                | keyCode == SDL.KeycodeKPMultiply = "NUM_MULTIPLY"
                                | keyCode == SDL.KeycodeKPDivide = "NUM_DIVIDE"
                                | keyCode == SDL.KeycodeKPMinus = "NUM_MINUS"
                                | keyCode == SDL.KeycodeKPPlus = "NUM_PLUS"
                                | keyCode == SDL.KeycodeKPEnter = "NUM_ENTER"
                                | keyCode == SDL.KeycodeKPComma = "NUM_COMMA"
                                | keyCode == SDL.KeycodeKPEnter = "NUM_ENTER"
                                | keyCode == SDL.KeycodeKP0 = "NUM_0"
                                | (keyNum >= 0x40000059 && keyNum <= 0x40000061) = "NUM_" ++ (show (keyNum-0x40000059+1))
                                | keyCode == SDL.KeycodePause = "BREAK"
                                | keyCode == SDL.KeycodeInsert = "INSERT"
                                | keyCode == SDL.KeycodeDelete = "DELETE"
                                | keyCode == SDL.KeycodeComma = "COMMA"
                                | keyCode == SDL.KeycodeSemicolon = "SEMICOLON"
                                | keyCode == SDL.KeycodePeriod = "PERIOD"
                                | keyCode == SDL.KeycodeColon = "COLON"
                                | keyCode == SDL.KeycodeLess = "LESS"
                                | keyCode == SDL.KeycodeMinus = "MINUS"
                                | keyCode == SDL.KeycodeHash = "HASH"
                                | keyCode == SDL.KeycodePlus = "PLUS"
                                | keyCode == SDL.KeycodeSlash = "SLASH"
                                | keyCode == SDL.KeycodeBackslash = "BACKSLASH"
                                | keyCode == SDL.KeycodeQuote = "GRAVE"
                                | keyCode == SDL.KeycodeBackquote = "CARET"
                                | keyCode == SDL.KeycodePrintScreen = "PRINT"
                                | keyCode == SDL.KeycodePageUp = "PAGE_UP"
                                | keyCode == SDL.KeycodePageDown = "PAGE_DOWN"
                                | keyCode == SDL.KeycodeEnd = "END"
                                | keyCode == SDL.KeycodeHome = "HOME"
                                | keyCode == SDL.KeycodeUp = "UP"
                                | keyCode == SDL.KeycodeDown = "DOWN"
                                | keyCode == SDL.KeycodeLeft = "LEFT"
                                | keyCode == SDL.KeycodeRight = "RIGHT"
                                | keyCode == SDL.KeycodeNumLockClear = "NUM_LOCK"
                                | keyCode == SDL.KeycodeScrollLock = "SCROLL_LOCK"
                                | otherwise = "" in
                        (key, if pressed then True else False)
                    else ("", True)
                _ -> ("", True)
        newInternKeyState = InternalKeyState {pressedI=pK, heldI=hK, releasedI=rK} 
            where (pK1, rK1) = foldr (\ev (accP, accR) -> case eventKey ev of {("",_) -> (accP, accR); 
                                                                               (k,True) -> ((k,timeMillis) : accP, accR); 
                                                                               (k,False) -> (accP, (k,timeMillis) : accR)}) ([],[]) events
                  (pK2, rK) = (nub pK1, nub rK1)
                  rKs = map (\(key,time) -> key) rK
                  rKolds = map (\(key,time) -> key) (releasedI internKeyState)
                  hKolds = map (\(key,time,dur) -> key) (heldI internKeyState)
                  hK = nub $ (map (\(key,start,dur) -> (key,start,(timeMillis-start))) (filter (\(key,start,dur) -> not $ key `elem` rKs) (heldI internKeyState))) ++ 
                             (map (\(key,tStart) -> (key, timeMillis, 0)) (filter (\(key,tStart) -> not $ key `elem` (rKolds++rKs)) (pressedI internKeyState)))
                  pK = filter (\(key,tStart) -> not $ key `elem` hKolds) pK2
    
    let newKeyState = KeyState {pressed=(pressedI newInternKeyState), released=(releasedI newInternKeyState), held=(map (\(key,start,dur) -> (key,dur)) (heldI newInternKeyState))}
        quit = ((map (\(key,_) -> key) (pressed newKeyState)) == ["ESC"]) && ("L-SHIFT" `elem` (map (\(key,_) -> key) (held newKeyState)))

    atomically $ writeTChan channel $ EventT "SDL_KEY_DATA" $ newKeyState
    threadDelay 33000
    unless quit (sdlKeyEventGen channel newInternKeyState startTime)
