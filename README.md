port module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attrib
import Random exposing (..)
import Svg as Svg
import Svg.Attributes exposing (..)
import Json.Decode as Decode
import Task exposing (..)
import Browser exposing (..)
import Time exposing (..)

type alias Game = 
    { direction : Direction
    , dimensions : Dimension
    , snake : Snake
    , isDead : Bool
    , ateApple : Bool
    , apple : Maybe Block
    , paused : Bool
    , score : Int
    , gameStarted : Bool
    }

type alias Dimension =
    {height : Int
    , width : Int
    }

type Direction 
    = Up 
    | Down
    | Left 
    | Right
    | Other
    | Space

type Snake = Snake (List Block)


type alias Block = 
    { x : Int
    , y : Int 
    }

type alias AppleSpawn =
    { position : (Int, Int)
    , chance : Int
    }

initSnake : Snake
initSnake = 
    let
        initDimX = 25
        initDimY = 25
    in
    Snake [ Block initDimX initDimY, 
        Block (initDimX) (initDimY+1), 
        Block (initDimX) (initDimY+2)
        ]

init : () -> (Game, Cmd Msg)
init _ =
    ({ direction = Down
    , dimensions =
        { height = 400
        , width = 500
        }
    , snake = initSnake 
    , isDead = False
    , ateApple = False
    , apple = Nothing
    , paused = False
    , score = 0
    , gameStarted = False
    }
    , initCmds
    )

-- update

type Msg
    = ArrowPressed Direction
    | Tick Posix
    | MaybeSpawnApple AppleSpawn

update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        ArrowPressed key ->
            let
                nGame= updateDirection key game
                newGame = {nGame | gameStarted = True }
            in
            updateGame newGame

        Tick _ ->
            updateGame game
        
        MaybeSpawnApple applespawn ->
            let
                newGame = generateApple game applespawn
            in
            updateGame newGame
    
generateApple : Game -> AppleSpawn -> Game
generateApple game app = 
    let
        (x, y) = 
            app.position
    in
    {game | apple = Just { x = x, y = y}}

updateGame : Game -> (Game, Cmd Msg)
updateGame game = 
    if game.isDead || game.paused then
        (game, Cmd.none)
    else 
        (game, Cmd.none)
            |> checkIfOutofBounds
            |> checkIfEatenSelf
            |> checkIfAteApple
            |> updateSnake
            |> updateApple
    
checkIfEatenSelf : (Game, Cmd Msg) -> (Game, Cmd Msg)
checkIfEatenSelf (game, cmd) = 
    let
        snakeList = 
            case game.snake of
                Snake s ->
                    s
        head = 
            snakeHead game.snake
        
        tail =
            List.drop 1 snakeList
        
        isDead = 
            game.isDead || List.any (samePosition head) tail
    in
    ({game | isDead = isDead}, cmd)

snakeHead : Snake -> Block
snakeHead (Snake snake) =
    List.head snake
        |> Maybe.withDefault { x = 0, y = 0 }


samePosition : Block -> Block -> Bool
samePosition a b = 
    a.x == b.x && a.y == b.y

checkIfAteApple : (Game, Cmd Msg) -> (Game, Cmd Msg)
checkIfAteApple (game, cmd) = 
    let
        head = snakeHead game.snake
    in
    case game.apple of
        Nothing ->
            ( {game | ateApple  = False}, cmd)
    
        Just apple ->
            let
                ate = samePosition head apple
                newScore = if ate then game.score+10 else game.score
                newGam = {game | ateApple = ate } 
                newGame = {newGam | score = newScore}
            in
            (  newGame , cmd)


checkIfOutofBounds : (Game, Cmd Msg) -> (Game, Cmd Msg)
checkIfOutofBounds (game, cmd) =
    let
        head = snakeHead game.snake

        isDead = 
            (head.x == 0 && game.direction == Left)
            || (head.y == 0 && game.direction == Up)
            || (head.x == 49 && game.direction == Right)
            || (head.y == 49 && game.direction == Down)

    in
    ({game | isDead = isDead}, cmd)

updateApple : ( Game, Cmd Msg ) -> ( Game, Cmd Msg )
updateApple (game, cmd) =
    case game.apple of
        Nothing ->
            let
                chance = Random.int 0 9

                x = Random.int 0 49

                y = Random.int 0 49

                pos = Random.pair x y
            in
            (game, Random.generate MaybeSpawnApple makeAppleSpawnGenerator)
        
        Just apple ->
            if game.ateApple then
                ({game | apple = Nothing}, cmd)
            else
                (game, cmd)

makeAppleSpawnGenerator : Random.Generator AppleSpawn
makeAppleSpawnGenerator = 
    let
        spawnPosition = 
            Random.pair (Random.int 0 49) (Random.int 0 49)

        spawnChance = (Random.int 0 9) 
    in
    Random.map2 (\pos chance -> { position = pos, chance = chance}) spawnPosition spawnChance


updateSnake : (Game, Cmd Msg) -> (Game, Cmd Msg)
updateSnake (game, cmd) = 
    let
        listBlock = (getListBlock game.snake)

        listBlockLength = List.length listBlock

        head = 
            snakeHead game.snake

        tail = 
            List.drop listBlockLength listBlock

        head1 = 
            case game.direction of
                Up ->
                    { head | y = head.y - 1 }
                
                Down ->
                    { head | y = head.y + 1 }
                
                Left -> 
                    { head | x = head.x - 1 }

                Right ->
                    { head | x = head.x + 1 }
                
                _ ->
                    head
        
        tailPositions = 
            if game.ateApple then
                let
                    lastElemMod = 
                        case tail of
                            d :: xs ->
                                case game.direction of
                                    Up ->
                                        [{ d | y = head.y - 1 }]
                                    
                                    Down ->
                                        [{ d| y = head.y + 1 }]
                                    
                                    Left -> 
                                        [{ d| x = head.x - 1 }]

                                    Right ->
                                        [{ d| x = head.x + 1 }]
                                    
                                    _ ->
                                        [d]
                            [] -> 
                                []
                in
                Snake <| (List.take listBlockLength listBlock) ++ lastElemMod
            
            else
                game.snake
            
        tailXs = movePosition game.direction (List.map .x <| getListBlock tailPositions)

        tailYs = movePosition game.direction (List.map .y <| getListBlock tailPositions)

        tail1 = List.map2 Block tailXs tailYs
    in
    
    if game.isDead then
        (game, cmd)

    else
        ({game | snake = Snake (head1 :: tail1) }, cmd)

updateDirection : Direction-> Game -> Game
updateDirection dir game =
    { game | direction = dir }

initCmds : Cmd Msg
initCmds = 
    Cmd.none

movePosition : Direction -> List Int -> List Int
movePosition dir list = 
    case dir of 
        Up ->
            List.map (\y -> y - 1) list
        
        Down -> 
            List.map (\y -> y + 1) list
        
        Left -> 
            List.map (\x -> x - 1) list
        
        Right ->
            List.map (\x -> x + 1) list
        
        _ -> 
            list
        

getListBlock: Snake -> List Block
getListBlock (Snake lis) =
    lis

toString : Int -> String
toString x = 
    String.fromInt x
    

-- VIEWS

view : Game -> Html Msg
view game = 
    div [Attrib.style "text-align" "center"] 
        [ div [Attrib.style "font-family" "sans-serif"]
            [p [Attrib.style "" ""] [text "Snake Game"]
            ]
        , div []
            [ render game ]
        , div []
                [text ("direction is: " ++ Debug.toString game.direction)]

        , div [] 
            [text <| "Score: " ++ toString game.score]
        ]
    

size : String
size = 
    "100"

backgroundColor : Svg.Attribute Msg
backgroundColor = 
    fill "#333133"

render : Game -> Html Msg
render game = 
    let
        (scaledWidth, scaledHeight) = 
            scale (game.dimensions.width, game.dimensions.height)           
    in
    Svg.svg 
        [ width scaledWidth, height scaledHeight, viewBox "0 0 50 50"]
        ([renderBackground game ]
            ++ renderSnake game.snake
            ++ renderApple game.apple 
        )

renderBackground : Game -> Svg.Svg Msg
renderBackground game =
    Svg.rect [ x "0", y "0", width size, height size, backgroundColor ] []


renderSnake : Snake -> List (Svg.Svg Msg)
renderSnake (Snake lis)=
    List.map (renderBlock "green") lis 


renderBlock : String -> Block -> Svg.Svg Msg
renderBlock colr block =
    let
        ( strX, strY ) =
            ( toString block.x, toString block.y )
    in
        Svg.rect [ x strX, y strY, width "1", height "1", fill colr, rx "0.2" ] []


renderApple : Maybe Block -> List (Svg.Svg Msg)
renderApple fruit =
    case fruit of
        Nothing ->
            []

        Just frut ->
            [ renderBlock "red" frut ]

scale : (Int, Int) -> ( String, String )
scale (width, height)=
    let
        toPixelStr =
            \i -> round i |> toString

        ( fWidth, fHeight ) =
            ( toFloat width, toFloat height )

        ( scaledX, scaledY ) =
            if fWidth > fHeight then
                ( fHeight / fWidth, 1.0 )
            else
                ( 1.0, fWidth / fHeight )
    in
        ( toPixelStr (fWidth * scaledX), toPixelStr (fHeight * scaledY) )

-- HANDLE KEYS

keyDecoder : Decode.Decoder Direction
keyDecoder =
  Decode.map toDirection Decode.string

toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" ->
        Left

    "ArrowRight" ->
        Right
    
    "ArrowDown" ->
        Down
    
    "ArrowUp" ->
        Up
    
    "Space" ->
        Space
    
    _ ->
      Other

decodeKey : Decode.Value -> Direction
decodeKey val =
    case (Decode.decodeValue keyDecoder val) of
        Ok dir ->
            dir 
        Err _ ->
            Other
        

-- port
port getKey : (Decode.Value -> msg ) -> Sub msg

keyPressed : (Direction -> msg) -> Sub msg
keyPressed toMsg = 
    getKey (\val -> toMsg (decodeKey val))

subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch [keyPressed ArrowPressed
        , if not game.paused then Time.every 1000 Tick else Sub.none]

main : Program () Game Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }