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

inScreen : (Int, Int) -> Positioned a -> Bool
inScreen (w, h) positioned =
    positioned.y < 800

-- TYPES

type alias Positioned a =
    { a
    | x : Int
    , y : Int
    , width : Int
    , height : Int
    }

type alias Shot = Positioned {}

type alias Ship = Positioned {}

type alias Invader = Positioned {}

type alias Game =
    { defender : Player
    , ship : Ship
    , shots : List Shot
    , window : (Int, Int)
    , invaders : List Invader
    , shift : Shift
    , shifted : Int
    , shield : List (Int, Int)
    }

type alias Player =
    { score : Int
    , lives : Int
    }

type Action
    = Click
    | Movement (Int, Int)
    | Resize (Int, Int)
    | Tick
    | EnemyTimer

type Shift
    = Left
    | None
    | Right

-- INIT

invaderWidth = 40
invaderHeight = 30

defaultGame : Game
defaultGame =
    { defender = Player 0 10
    , ship = createShip 0 0
    , shots = []
    , window = (0, 0)
    , invaders = createInvaders 7 4
    , shift = Left
    , shifted = 0
    , shield = [(0, -100), (-200, -100), (200, -100)]
    }

createInvaders : Int -> Int -> List Invader
createInvaders x y =
    List.append (List.map intToTupleX (positionInvaderX x)) (List.map intToTupleY (positionInvaderY y))
    |> List.map (\(x, y) -> createInvader x y)

createPositioned : Int -> Int -> Int -> Int -> Positioned {}
createPositioned width height x y =
    { x = x
    , y = y
    , width = width
    , height = height
    }

createShot : Int -> Int -> Shot
createShot =
    createPositioned 5 5

createShip : Int -> Int -> Ship
createShip =
    createPositioned 40 30

createInvader : Int -> Int -> Invader
createInvader =
    createPositioned 40 30

-- correct for Mouse <-> Collage discrepancy in NUll-points
convert : (Int, Int) -> (Int, Int) -> (Int, Int)
convert (w, h) (vx, vy) =
    (vx - w // 2, 15 - h // 2)

positionInvaderX : Int -> List Int
positionInvaderX x =
    List.map multList [-((x-1)//2)..((x-1)//2)]

positionInvaderY : Int -> List Int
positionInvaderY y =
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

---VIEWS

movePositioned : Positioned a -> Form -> Form
movePositioned positioned =
    move (toFloat positioned.x, toFloat positioned.y)

viewShip : Ship -> Form
viewShip ship =
    image ship.width ship.height "images/Ship.png"
    |> toForm
    |> movePositioned ship

viewShot : Shot -> Form
viewShot shot =
    rect (toFloat shot.width) (toFloat shot.height)
    |> filled Color.purple
    |> movePositioned shot

viewInvader : Invader -> Form
viewInvader invader =
    image invader.width invader.height "images/Invader.png"
    |> toForm
    |> movePositioned invader

viewShield : (Int, Int) -> Form
viewShield (x, y) =
    image 40 30 "images/shield.png"
    |> toForm
    |> move (toFloat x, toFloat y)

view : (Int, Int) -> Game -> Element
view (w, h) game =
    let
        defender = viewShip game.ship
        shots = List.map viewShot game.shots
        invaders = List.map viewInvader game.invaders
        shields = List.map viewShield game.shield
    in
        collage w h (defender :: shots ++ invaders ++ shields)

--- UPDATE
-- update Enemy position
wiggle : Game -> Game
wiggle game =
    case game.shift of
        Left ->
            { game
            | invaders <- List.map (moveBy (-5, 0)) game.invaders
            , shifted <- (game.shifted - 1)
            }
        Right ->
            { game
            | invaders <- List.map (moveBy (5, 0)) game.invaders
            , shifted <- (game.shifted + 1)
            }
        None ->
            game

changeShift : Game -> Game
changeShift game =
    { game
    | shift <-
        case game.shifted of
            -5 -> Right
            5 -> Left
            _ -> game.shift
    }

moveBy : (Int, Int) -> Positioned a -> Positioned a
moveBy (x, y) positioned =
    { positioned
    | x <- positioned.x + x
    , y <- positioned.y + y
    }

-- update view after event
update : Action -> Game -> Game
update action oldGame =
    case action of
        Click ->
            { oldGame
            | shots <-
                (createShot oldGame.ship.x (oldGame.ship.y + 15))
                    :: oldGame.shots
            }
        Movement (x, y) ->
            { oldGame
            | ship <- createShip x y
            }
        Resize    newSize ->
            { oldGame
            | window <- newSize
            }
        Tick ->
            let
                shots =
                    oldGame.shots
                    |> List.map (moveBy (0, 3))
                    |> List.filter (inScreen oldGame.window)
                invaders =
                    List.filterMap (doesntCollideWith shots) oldGame.invaders
            in
                { oldGame
                | shots <- shots
                , invaders <- invaders
                }
        EnemyTimer ->
            oldGame
            |> changeShift
            |> wiggle

-- COLLISION

collides : Positioned a -> Positioned b -> Bool
collides a b =
    False

doesntCollideWith : List (Positioned a) -> Positioned b -> Maybe (Positioned b)
doesntCollideWith shots ship =
    if not (List.any (collides ship) shots)
    then Just ship
    else Nothing

--- INPUTS
-- organize inputs
inputs : Signal Action
inputs =
    Signal.mergeMany
        [ Signal.map (always Click) Mouse.clicks
        , Signal.map Movement (Signal.map2 convert Window.dimensions Mouse.position)
        , Signal.map Resize Window.dimensions
        , Signal.map (always Tick) (Time.fps 30)
        , Signal.map (always EnemyTimer) (Time.fps 1)
        ]

state : Signal Game
state = Signal.foldp update defaultGame inputs

main : Signal Element
main = Signal.map2 view Window.dimensions state
