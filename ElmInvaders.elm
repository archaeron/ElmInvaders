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
  if y < 800 then True
  else False

-- objects & their wiggeling functions
type alias Game =
  { defender : Player
  , ship : Ship
  , shots : List (Int, Int)
  , window : (Int, Int)
  , enemies : List (Int, Int)
  , shift : Shift
  , shifted : Int
  , level : Level}

type alias Player =
  { score : Int
  , lives : Int }

type alias Ship =
  { position : (Int, Int)}

type Action = Click | Movement (Int, Int) | Resize (Int, Int) | ShotTimer | EnemyTimer

type Shift = Left | None | Right

type Level = One | Boss

defaultGame : Game
defaultGame =
  { defender = Player 0 10
  , ship = Ship (0, 0)
  , shots = []
  , window = (0,0)
  , enemies = createEnemies 7 4
  , shift = Left
  , shifted = 0
  , level = One}

ball : Int -> Int -> Form
ball vx vy =
  circle 5.0
  |> filled Color.blue
  |> move (toFloat vx, toFloat vy)

-- correct for Mouse <-> Collage discrepancy in NUll-points
convert : (Int, Int) -> (Int, Int) -> (Int, Int)
convert (w, h) (vx, vy) =
  (vx - w // 2, 15 - h // 2)

createEnemies : Int -> Int -> List (Int, Int)
createEnemies x y =
  List.append (List.map intToTupleX (positionEnemyX x)) (List.map intToTupleY (positionEnemyY y))

positionEnemyX : Int -> List Int
positionEnemyX x =
  List.map multList [-((x-1)//2)..((x-1)//2)]

positionEnemyY : Int -> List Int
positionEnemyY y =
  List.map multList [0..y]

multList : Int -> Int
multList a =
  a * 75

intToTupleX : Int -> (Int, Int)
intToTupleX a =
  (a, 0)

intToTupleY : Int -> (Int, Int)
intToTupleY b =
  (0, b)

---VIEW
-- create shown Element
viewShip : (Int, Int) -> Form
viewShip (vx, vy) =
  image 40 30 "images/Ship.png"
  |> toForm
  |> move (toFloat vx, toFloat vy)

viewShot : (Int, Int) -> Form
viewShot (x, y) =
  rect 5 5
  |> filled Color.purple
  |> move (toFloat x, toFloat y)

viewEnemy : (Int, Int) -> Form
viewEnemy (vx, vy) =
  image 40 30 "images/Invader.png"
  |> toForm
  |> move (toFloat vx, toFloat vy)

view : (Int, Int) -> Game -> Element
view (w, h) game =
  collage w h ((viewShip game.ship.position ::
    (List.map viewShot game.shots)) ++ (List.map viewEnemy game.enemies))

--- UPDATE
-- update Enemy position
wiggle : Game -> Game
wiggle oldGame =
  case oldGame.shift of
    Left ->
      { oldGame
      | enemies <- List.map (addTuples (-5, 0)) oldGame.enemies
      , shifted <- (oldGame.shifted - 1)
      }
    Right ->
      { oldGame
      | enemies <- List.map (addTuples (5, 0)) oldGame.enemies
      , shifted <- (oldGame.shifted + 1)
      }
    None ->
      { oldGame
      | enemies <- oldGame.enemies
      }
changeShift : Game -> Game
changeShift oldGame =
  { oldGame
  | shift <- if | oldGame.shifted == -5 -> Right
                | oldGame.shifted == 5 -> Left
                | otherwise -> oldGame.shift
  }

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
    ShotTimer ->
      { oldGame
      | shots <- List.filter (inScreen oldGame.window) (List.map (addTuples (0, 1)) oldGame.shots)
      }
    EnemyTimer ->
      wiggle (changeShift oldGame)

--- INPUTS
-- organize inputs
inputs : Signal Action
inputs =
  Signal.mergeMany
    [ Signal.map (always Click) Mouse.clicks
    , Signal.map Movement (Signal.map2 convert Window.dimensions Mouse.position)
    , Signal.map Resize Window.dimensions
    , Signal.map (always ShotTimer) (Time.fps 30)
    , Signal.map (always EnemyTimer) (Time.fps 1)
    ]

state : Signal Game
state = Signal.foldp update defaultGame inputs

main : Signal Element
main = Signal.map2 view Window.dimensions state
