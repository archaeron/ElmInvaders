module Main where
import Mouse
import Signal
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Color
import Time

-- general functions
addTuples : (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) =
  (a + c, b + d)

inScreen : (Int, Int) -> (Int, Int) -> Bool
inScreen (w, h) (x, y) =
  if y < h then True
  else False

-- objects & their wiggeling functions
type alias Game =
  { defender : Player
  , ship : Ship
  , shots : List (Int, Int)
  , window : (Int, Int)}

type alias Player =
  { score : Int
  , lives : Int }

type alias Ship =
  { position : (Int, Int)}

type Action = Click | Movement (Int, Int) | Resize (Int, Int) | Timer

defaultGame : Game
defaultGame =
  { defender = Player 0 10
  , ship = Ship (0, 0)
  , shots = []
  , window = (10,10)}

ball : Int -> Int -> Form
ball vx vy =
  circle 5.0
  |> filled Color.blue
  |> move (toFloat vx, toFloat vy)

-- correct for Mouse <-> Collage discrepancy in NUll-points
convert : (Int, Int) -> (Int, Int) -> (Int, Int)
convert (w, h) (vx, vy) =
  (vx - w // 2, 15 - h // 2)

---VIEW
-- create shown Element
viewShip : (Int, Int) -> Form
viewShip (vx, vy) =
  image 40 30 "images/Ship.png"
  |> toForm
  |> move (toFloat vx, toFloat vy)

viewShot : (Int, Int) -> Form
viewShot (x, y)=
  rect 5 5
  |> filled Color.purple
  |> move (toFloat x, toFloat y)

view : (Int, Int) -> Game -> Element
view (w, h) game =
  collage w h (viewShip game.ship.position :: (List.map viewShot game.shots))

--- UPDATE
-- update view after event
update : Action -> Game -> Game
update action oldGame =
  case action of
    Click ->
      { oldGame
      | shots <- (addTuples oldGame.ship.position (0, 15)) :: oldGame.shots
      }
    Movement newPosition ->
      { oldGame
      | ship <- Ship newPosition
      }
    Resize  newSize ->
      { oldGame
      | window <- newSize
      }
    Timer ->
      { oldGame
      | shots <- List.filter (inScreen oldGame.window) (List.map (addTuples (0, 1)) oldGame.shots)
      }

--- INPUTS
-- organize inputs
inputs : Signal Action
inputs =
  Signal.mergeMany
    [ Signal.map (always Click) Mouse.clicks
    , Signal.map Movement (Signal.map2 convert Window.dimensions Mouse.position)
    , Signal.map Resize Window.dimensions
    , Signal.map (always Timer) (Time.fps 30)
    ]

state : Signal Game
state = Signal.foldp update defaultGame inputs

main : Signal Element
main = Signal.map2 view Window.dimensions state
