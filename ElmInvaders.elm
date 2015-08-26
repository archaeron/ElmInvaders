module Main where
import Mouse
import Signal
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Color
import Time
import Random
import Array
import Random.Array

--- GENERAL FUNCTIONS
-- addition of tuples
addTuples : (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (a, b) (c, d) =
  (a + c, b + d)

-- check if a tuple b is in window tuple a !!not finished, window size from start missing!!
inScreen : (Int, Int) -> (Int, Int) -> Bool
inScreen (w, h) (x, y) =
  if y < 800 && y > -800 then True
  else False

-- correct for Mouse <-> Collage discrepancy in NUll-points
convert : (Int, Int) -> (Int, Int) -> (Int, Int)
convert (w, h) (vx, vy) =
  (vx - w // 2, 15 - h // 2)

-- position enemies matrix-like -> save positions in List
createEnemies : Int -> Int -> List (Int, Int)
createEnemies x y =
  [(0,0),(-100,0),(-200,0),(100,0),(200,0),(0,100),(-100,100),(-200,100),(100,100),(200,100),(0,200),(-100,200),(-200,200),(100,200),(200,200)]

-- positions shields 50 px above the ship
position : (Int, Int) -> (Int, Int) -> (Int, Int)
position (x, y) (ex, ey)=
  (ex, y + 50)

-- create position of new enemy shot
newEnemyShot : List (Int, Int) -> Random.Seed -> List (Int, Int)
newEnemyShot enemies seed =
  takeSample (Random.Array.sample seed (Array.fromList enemies))

takeSample : (Maybe (Int, Int), Random.Seed) -> List (Int, Int)
takeSample (a, seed) =
  case a of
    Just a -> [a]
    Nothing -> []

-- create new seed for randomness
newSeed : List (Int, Int) -> Random.Seed -> Random.Seed
newSeed enemies seed =
  extractSeed (Random.Array.sample seed (Array.fromList enemies))

extractSeed : (Maybe (Int, Int), Random.Seed) -> Random.Seed
extractSeed (a, seed) =
  seed

limit : Int -> Int
limit vx =
  if  | vx <= 200 && vx >= -200 -> vx
      | vx < -200 -> -200
      | vx > 200 -> 200


--- TYPE DEFINITIONS
-- objects & their wiggeling functions
type alias Game =
  { defender : Player
  , ship : Ship
  , shotsShip : List (Int, Int)
  , shotsEnemies : List (Int, Int)
  , seed : Random.Seed
  , enemies : List (Int, Int)
  , shift : Shift
  , shifted : Int
  , level : Level
  , shield : List (Int, Int)
  , window : (Int, Int)
  , playfieldWidth : Int}

type alias Player =
  { score : Int
  , lives : Int }

type alias Ship =
  { position : (Int, Int)}

type Action = Click | Movement (Int, Int) | Resize (Int, Int) | ShotTimer | EnemyTimer | EnemyShotTimer

type Shift = Left | None | Right

type Level = One | Boss

--- INITIALIZE VARIABLES
defaultGame : Game
defaultGame =
  { defender = Player 0 10
  , ship = Ship (0, -200)
  , shotsShip = []
  , shotsEnemies = []
  , seed = Random.initialSeed 8
  , window = (0,0)
  , enemies = createEnemies 7 4
  , shift = Left
  , shifted = 0
  , level = One
  , shield = [(0, -100), (-200, -100), (200, -100)]
  , playfieldWidth = 200}

ball : Int -> Int -> Form
ball vx vy =
  circle 5.0
  |> filled Color.blue
  |> move (toFloat vx, toFloat vy)

---VIEW CREATION
-- create shown Element
viewShip : (Int, Int) -> Form
viewShip (vx, vy) =
  croppedImage (0, 0) 22 22 "images/sprite.gif"
  |> toForm
  |> move (toFloat (limit vx), toFloat vy)

viewFShot : (Int, Int) -> Form
viewFShot (x, y) =
  croppedImage (66, 0) 22 22 "images/sprite.gif"
  |> toForm
  |> move (toFloat x, toFloat y)

viewEShot : (Int, Int) -> Form
viewEShot (x, y) =
  rect 5 5
  |> filled Color.red
  |> move (toFloat x, toFloat y)

viewEnemy : (Int, Int) -> Form
viewEnemy (vx, vy) =
  croppedImage (22, 0) 22 22 "images/sprite.gif"
  |> toForm
  |> move (toFloat vx, toFloat vy)

viewShield : (Int, Int) -> Form
viewShield (x, y) =
  croppedImage (44, 0) 22 22 "images/sprite.gif"
  |> toForm
  |> move (toFloat x, toFloat y)

viewBg : Int -> Int -> Form
viewBg w h =
  image w h "images/bg.png"
  |> toForm

view : (Int, Int) -> Game -> Element
view (w, h) game =
  collage w h ((viewBg w h :: (List.map viewEnemy game.enemies)) ++ (viewShip game.ship.position ::
    (List.map viewFShot game.shotsShip)) ++ (List.map viewShield game.shield) ++ (List.map viewEShot game.shotsEnemies))

--- MOVEMENTS
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

-- change shifting direction of enemies
changeShift : Game -> Game
changeShift oldGame =
  { oldGame
  | shift <- if | oldGame.shifted == -5 -> Right
                | oldGame.shifted == 5 -> Left
                | otherwise -> oldGame.shift
  }

--- COLLISION
-- returns True of there is no collision, False otherwise
noCollision : List (Int, Int) -> (Int, Int) -> Bool
noCollision shotsShip object =
  if List.member True (List.map2 collisionList shotsShip (List.repeat (List.length shotsShip) object)) then False
    else True

collisionList : (Int, Int) -> (Int, Int) -> Bool
collisionList (sx, sy) (ox, oy) =
  if ox - 20 < sx && sx < ox + 20 && oy - 15 < sy && sy < oy + 15 then True
    else False

--- UPDATE GAME
-- update view after event
update : Action -> Game -> Game
update action oldGame =
  case action of
    Click ->
      { oldGame
      | shotsShip <- (addTuples oldGame.ship.position (0, 15)) :: oldGame.shotsShip
      }
    Movement newPosition ->
      { oldGame
      | ship <- Ship newPosition
      , shield <- List.map (position newPosition) oldGame.shield
      }
    Resize  newSize ->
      { oldGame
      | window <- newSize
      }
    ShotTimer ->
      { oldGame
      | enemies <- List.filter (noCollision (List.map (addTuples (0, 1)) oldGame.shotsShip)) oldGame.enemies
      , shield <- List.filter (noCollision (List.map (addTuples (0, 1)) oldGame.shotsShip)) oldGame.shield
      , shotsShip <- List.filter (noCollision (oldGame.enemies ++ oldGame.shield)) (List.filter (inScreen oldGame.window) (List.map (addTuples (0, 1)) oldGame.shotsShip))
      , shotsEnemies <- List.filter (noCollision oldGame.shield) (List.map (addTuples (0, -1)) (List.filter (inScreen oldGame.window) oldGame.shotsEnemies))
      }
    EnemyTimer ->
      wiggle (changeShift oldGame)
    EnemyShotTimer ->
      { oldGame
      | shotsEnemies <- (newEnemyShot oldGame.enemies oldGame.seed) ++ oldGame.shotsEnemies
      , seed <- newSeed oldGame.enemies oldGame.seed
      }

--- INPUTS & SIGNALS
inputs : Signal Action
inputs =
  Signal.mergeMany
    [ Signal.map (always Click) Mouse.clicks
    , Signal.map Movement (Signal.map2 convert Window.dimensions Mouse.position)
    , Signal.map Resize Window.dimensions
    , Signal.map (always ShotTimer) (Time.fps 30)
    , Signal.map (always EnemyTimer) (Time.fps 1)
    , Signal.map (always EnemyShotTimer) (Time.fps 1)
    ]

state : Signal Game
state = Signal.foldp update defaultGame inputs

main : Signal Element
main = Signal.map2 view Window.dimensions state
