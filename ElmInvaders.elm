module Main where
import Mouse
import Signal
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Color


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

type Action = Click | Movement (Int, Int) | Resize (Int, Int)

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

ship : Ship
ship =
  { position = (0, 0)}

-- correct for Mouse <-> Collage discrepancy in NUll-points
convert : (Int, Int) -> (Int, Int) -> (Int, Int)
convert (w, h) (vx, vy) =
  (vx - w // 2, h // 2 - vy)

---VIEW
-- create shown Element
viewShip : (Int, Int) -> (Int, Int) -> Form
viewShip (w, h) (vx, vy) =
  image 40 30 "images/Ship.png"
  |> toForm
  |> move (toFloat (vx - w // 2), toFloat (15 - h // 2))

viewShot : (Int, Int) -> Form
viewShot (x,y)=
  rect 5 400
  |> filled Color.purple
  |> move (toFloat x, toFloat y)

view : (Int, Int) -> Game -> Element
view (w, h) game =
  collage w h (viewShip (w, h) game.ship.position :: (List.map viewShot game.shots))


-- update view after event
update : Action -> Game -> Game
update action oldGame =
  case action of
    Click ->
      { oldGame
      | shots <- ship.position :: oldGame.shots
      }
    Movement newPosition ->
      { oldGame
      | ship <-
        { ship
        | position <- newPosition
        }
      }
    Resize  newSize->
      { oldGame
      | window <- newSize
      }

inputs : Signal Action
inputs =
  Signal.mergeMany
    [ Signal.map (always Click) Mouse.clicks
    , Signal.map Movement Mouse.position
    , Signal.map Resize Window.dimensions
    ]

state : Signal Game
state = Signal.foldp update defaultGame inputs

main : Signal Element
main = Signal.map2 view Window.dimensions state
