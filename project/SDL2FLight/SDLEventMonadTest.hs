import Network.MateLight.Simple
import Network.MateLight

import SDLEventProvider.SDLKeyEventProvider

import Control.Monad.State
import Control.Monad.Reader

import Data.Maybe
import Data.Typeable
import qualified Network.Socket as Sock

type KeyStatus = (String, String, Integer) -- Represents the tuple (KeyStatusString, KeyNameString, Time) 

move :: (Int, Int) -> KeyStatus -> (Int, Int) -> (Int, Int)
move (xdim, ydim) ("Pressed","P0_Axis_1_D0",_) (x,y)    = (x, (y - 1) `mod` ydim)
move (xdim, ydim) ("Held","P0_Axis_1_D0",dur) (x,y)     = if dur >= 100 then (x, (y - 1) `mod` ydim) else (x,y)
move (xdim, ydim) ("Pressed","P0_Axis_0_D0",_) (x,y)    = ((x - 1) `mod` xdim, y)
move (xdim, ydim) ("Held","P0_Axis_0_D0",dur) (x,y)     = if dur >= 100 then ((x - 1) `mod` xdim, y) else (x,y)
move (xdim, ydim) ("Pressed","P0_Axis_1_D1",_) (x,y)    = (x, (y + 1) `mod` ydim)
move (xdim, ydim) ("Held","P0_Axis_1_D1",dur) (x,y)     = if dur >= 100 then (x, (y + 1) `mod` ydim) else (x,y)
move (xdim, ydim) ("Pressed","P0_Axis_0_D1",_) (x,y)    = ((x + 1) `mod` xdim, y)
move (xdim, ydim) ("Held","P0_Axis_0_D1",dur) (x,y)     = if dur >= 100 then ((x + 1) `mod` xdim, y) else (x,y)

move (xdim, ydim) ("Pressed","UP",_) (x,y)              = (x, (y - 1) `mod` ydim)
move (xdim, ydim) ("Held","UP",dur) (x,y)               = if dur >= 100 then (x, (y - 1) `mod` ydim) else (x,y)
move (xdim, ydim) ("Pressed","LEFT",_) (x,y)            = ((x - 1) `mod` xdim, y)
move (xdim, ydim) ("Held","LEFT",dur) (x,y)             = if dur >= 100 then ((x - 1) `mod` xdim, y) else (x,y)
move (xdim, ydim) ("Pressed","DOWN",_) (x,y)            = (x, (y + 1) `mod` ydim)
move (xdim, ydim) ("Held","DOWN",dur) (x,y)             = if dur >= 100 then (x, (y + 1) `mod` ydim) else (x,y)
move (xdim, ydim) ("Pressed","RIGHT",_) (x,y)           = ((x + 1) `mod` xdim, y)
move (xdim, ydim) ("Held","RIGHT",dur) (x,y)            = if dur >= 100 then ((x + 1) `mod` xdim, y) else (x,y)
move _ _ (x,y) = (x,y)

toFrame :: (Int, Int) -> (Int, Int) -> ListFrame
toFrame (xdim, ydim) (x', y') = ListFrame $ map (\y -> map (\x -> if x == x' && y == y' then Pixel 0xff 0xff 0xff else Pixel 0 0 0) [0 .. xdim - 1]) [0 .. ydim - 1]

getKeyDataTuples (Just keyState) = (map (\(k,t) -> ("Pressed",k,t)) (pressed $ keyState)) ++ 
                                   (map (\(k,d) -> ("Held",k,d)) (held $ keyState)) ++ 
                                   (map (\(k,t) -> ("Released",k,t)) (released $ keyState))
getKeyDataTuples _ = []  
               
getButtonDataTuples (Just buttonState) = (map (\(k,t) -> ("Pressed",k,t)) (pressedB $ buttonState)) ++ 
                                         (map (\(k,d) -> ("Held",k,d)) (heldB $ buttonState)) ++ 
                                         (map (\(k,t) -> ("Released",k,t)) (releasedB $ buttonState))
getButtonDataTuples _ = []     

eventTest :: [EventT] -> MateMonad ListFrame (Int,Int) IO ListFrame
eventTest events = do 
        state <- get
        let state' = foldl (\acc (EventT mod ev) -> case mod of
                                        "SDL_KEY_DATA" -> foldl (\accState key -> move dim key accState) acc (getKeyDataTuples ((cast ev) :: Maybe KeyState))
                                        "SDL_JOYSTICK_DATA" -> foldl (\accState key -> move dim key accState) acc (getButtonDataTuples ((cast ev) :: Maybe ButtonState))
                                        otherwise -> acc) state events
        put $ state'
        return (toFrame dim state')
  
dim :: (Int, Int)
dim = (30, 12)
  
main :: IO ()
main = do
    window <- showSDLControlWindow
    Sock.withSocketsDo $ runMateM (Config (fromJust $ parseAddress "134.28.70.51") 1337 dim (Just 33000) True [sdlKeyEventProvider, sdlJoystickEventProvider]) eventTest (0,0)
    destroySDLControlWindow window
