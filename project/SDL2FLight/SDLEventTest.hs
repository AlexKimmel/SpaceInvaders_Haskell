import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Network.MateLight
import Network.MateLight.Simple
import qualified Network.Socket as Sock
import SDLEventProvider.SDLKeyEventProvider

--                status  key     time
type KeyStatus = (String, String, Integer)

dim :: (Int, Int) -- wall dimensions
dim = (30, 12)

getKeyDataTuples keyState =
  map (\(k, t) -> ("Pressed", k, t)) (pressed keyState)
    ++ map (\(k, d) -> ("Held", k, d)) (held keyState)
    ++ map (\(k, t) -> ("Released", k, t)) (released keyState)

getButtonDataTuples buttonState =
  map (\(k, t) -> ("Pressed", k, t)) (pressedB buttonState)
    ++ map (\(k, d) -> ("Held", k, d)) (heldB buttonState)
    ++ map (\(k, t) -> ("Released", k, t)) (releasedB buttonState)

-- BEGIN MAIN FUNCTIONALITY

type MyState = (Int, Int)

--      wall dimens   input event  state      new state
move :: (Int, Int) -> KeyStatus -> MyState -> MyState
move (xdim, ydim) ("Pressed", "P0_Axis_1_D0", _) (x, y) = (x, (y - 1) `mod` ydim)
move (xdim, ydim) ("Held", "P0_Axis_1_D0", dur) (x, y) = if dur >= 100 then (x, (y - 1) `mod` ydim) else (x, y)
move (xdim, ydim) ("Pressed", "P0_Axis_0_D0", _) (x, y) = ((x - 1) `mod` xdim, y)
move (xdim, ydim) ("Held", "P0_Axis_0_D0", dur) (x, y) = if dur >= 100 then ((x - 1) `mod` xdim, y) else (x, y)
move (xdim, ydim) ("Pressed", "P0_Axis_1_D1", _) (x, y) = (x, (y + 1) `mod` ydim)
move (xdim, ydim) ("Held", "P0_Axis_1_D1", dur) (x, y) = if dur >= 100 then (x, (y + 1) `mod` ydim) else (x, y)
move (xdim, ydim) ("Pressed", "P0_Axis_0_D1", _) (x, y) = ((x + 1) `mod` xdim, y)
move (xdim, ydim) ("Held", "P0_Axis_0_D1", dur) (x, y) = if dur >= 100 then ((x + 1) `mod` xdim, y) else (x, y)
move (xdim, ydim) ("Pressed", "UP", _) (x, y) = (x, (y - 1) `mod` ydim)
move (xdim, ydim) ("Held", "UP", dur) (x, y) = if dur >= 100 then (x, (y - 1) `mod` ydim) else (x, y)
move (xdim, ydim) ("Pressed", "LEFT", _) (x, y) = ((x - 1) `mod` xdim, y)
move (xdim, ydim) ("Held", "LEFT", dur) (x, y) = if dur >= 100 then ((x - 1) `mod` xdim, y) else (x, y)
move (xdim, ydim) ("Pressed", "DOWN", _) (x, y) = (x, (y + 1) `mod` ydim)
move (xdim, ydim) ("Held", "DOWN", dur) (x, y) = if dur >= 100 then (x, (y + 1) `mod` ydim) else (x, y)
move (xdim, ydim) ("Pressed", "RIGHT", _) (x, y) = ((x + 1) `mod` xdim, y)
move (xdim, ydim) ("Held", "RIGHT", dur) (x, y) = if dur >= 100 then ((x + 1) `mod` xdim, y) else (x, y)
move _ _ (x, y) = (x, y)

--         wall dimens   state      generated frame
toFrame :: (Int, Int) -> MyState -> ListFrame
toFrame (xdim, ydim) (x', y') =
  ListFrame $
    map
      ( \y ->
          map
            (\x -> if x == x' && y == y' then Pixel 0xff 0xff 0xff else Pixel 0 0 0)
            [0 .. xdim - 1]
      )
      [0 .. ydim - 1]

--           input events      state      frame       new state
eventTest :: [Event String] -> MyState -> (ListFrame, MyState)
eventTest events state = (toFrame dim state', state')
  where
    state' =
      foldl
        ( \acc (Event mod ev) -> case mod of
            "SDL_KEY_DATA" ->
              foldl (flip (move dim)) acc (getKeyDataTuples (read ev :: KeyState))
            "SDL_JOYSTICK_DATA" ->
              foldl (flip (move dim)) acc (getButtonDataTuples (read ev :: ButtonState))
            _ ->
              acc
        )
        state
        events

-- END MAIN FUNCTIONALITY
-- BEGIN CONFIGURATION

ip :: String -- IP address of wall/emulator
ip = "134.28.70.51" -- "127.0.0.1"

delay :: Int -- delay between frames in microseconds
delay = 33000

initState :: MyState -- initial state
initState = (0, 0)

-- END CONFIGURATION

main :: IO ()
main = do
  window <- showSDLControlWindow
  Sock.withSocketsDo $
    runMate
      (Config (fromJust $ parseAddress ip) 1337 dim (Just delay) True [sdlKeyEventProvider, sdlJoystickEventProvider])
      eventTest
      initState
  destroySDLControlWindow window
