module Main where

import Color
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input
import Random
import Time
import Touch

port windowWidth : Signal Int
port windowHeight : Signal Int

port windowDimensions : Signal (Int, Int)
port windowDimensions = Signal.map2 (\x y -> (x, y)) windowWidth windowHeight

-- MODEL

type Color = Yellow | Blue | Orange | Green | Red
type Shape = Circle | X | Square | Triangle | Star

type alias Icon = {
    color : Color
  , shape : Shape
  }

allPossibleColors : List Color
allPossibleColors = [ Yellow, Blue, Orange, Green, Red ]

allPossibleShapes : List Shape
allPossibleShapes = [ Circle, X, Square, Triangle, Star ]

{-
icons : List Icon
icons = [
    Icon Yellow Circle
  , Icon Blue X
  , Icon Orange Square
  , Icon Green Triangle
  , Icon Red Star
  ]
-}

--type PauseState = {
    --mal richtig ueberlegen, was die states sind
  --}

type State = Intro | Pause Bool Bool | Running

type alias Model = {
    state : State
  , icons : List Icon
  , icon : Icon
  , hintIcons : (Icon, Icon)
  , points1 : Int
  , points2 : Int
  }

{-| splitAt 2 [1,2,3,4,5,6] === ([1,2],[3,4,5,6]) -}
splitAt : Int -> List a -> (List a, List a)
splitAt n l = case (n, l) of
                (0, xs)     -> ([], xs)
                (_, [])     -> ([], [])
                (n, (x::xs)) ->
                  let (xs', xs'') = splitAt (n - 1) xs
                  in (x::xs', xs'')


init : List a -> List a
init l = case l of
           [x] -> []
           (x::xs) -> x :: init xs
           _ -> Debug.crash "init failed"

nthElement : Int -> List a -> a
nthElement n = List.drop n >> unsafeHead

unsafeHead : List a -> a
unsafeHead xs = case xs of
  (x::_) -> x
  _ -> Debug.crash "unsafeHead with empty list"

{-| Get last element of a list. (not total) -}
last : List a -> a
last = List.reverse >> unsafeHead

{-| Suffle a list using random numbers
The length of the list of random numbers has to be larger
as long as the list that shall be shuffled. -}
shuffleHelper : List Int -> List a -> List a
shuffleHelper randoms l =
  case randoms of
    (i::is) ->
      case l of
        (x::xs) ->
          let (firsts, rest) = splitAt (i % List.length l + 1) l
          in (last firsts) :: shuffleHelper is (init firsts ++ rest)
        x -> x
    _ -> Debug.crash "shuffle failed"

shuffle : Random.Seed -> List a -> (List a, Random.Seed)
shuffle seed xs =
  let
    intGenerator = Random.int Random.minInt Random.maxInt
    listGenerator = Random.list (List.length xs + 1) intGenerator
    (randomNumbers, seed') = Random.generate listGenerator seed
  in
    (shuffleHelper randomNumbers xs, seed')

createNewIcons : Random.Seed -> (List Icon, Random.Seed)
createNewIcons seed0 =
  let
    (colors, seed1) = shuffle seed0 allPossibleColors
    (shapes, seed2) = shuffle seed1 allPossibleShapes
  in
    (List.map2 Icon colors shapes, seed2)

newIconWithHints : List Icon -> Random.Seed -> (Icon, (Icon, Icon), Random.Seed)
newIconWithHints icons seed0 =
  let
    (shuffledIcons, seed1) = shuffle seed0 icons
    icon = nthElement 0 shuffledIcons
    remainingIcon0 = nthElement 1 shuffledIcons
    remainingIcon1 = nthElement 2 shuffledIcons
    remainingIcon2 = nthElement 3 shuffledIcons
    remainingIcon3 = nthElement 4 shuffledIcons
    hint0 = Icon remainingIcon0.color remainingIcon2.shape
    hint1 = Icon remainingIcon1.color remainingIcon3.shape
  in
    (icon, (hint0, hint1), seed1)

introModel : Model
introModel = {
    state = Intro
  , icons = [
        Icon Yellow Circle
      , Icon Blue X
      , Icon Orange Square
      , Icon Green Triangle
      , Icon Red Star
      ]
  , icon = Icon Yellow Circle
  , hintIcons = (Icon Blue Square, Icon Green Star)
  , points1 = 0
  , points2 = 0 }


-- UPDATE

type Action = Start | P1Tab Icon | P2Tab Icon | P1Ready | P2Ready

type alias Input = { action : Action, seed : Random.Seed }

update : Input -> Model -> Model
update {action, seed} model =
  case action of
    Start ->
      let
        (newIcons, seed') = createNewIcons seed
        (newIcon, newHints, _) = newIconWithHints newIcons seed'
      in
        { state = Running
        , icons = newIcons
        , icon = newIcon
        , hintIcons = newHints
        , points1 = fst (Random.generate(Random.int 12 1000) seed)
        , points2 = 12 } -- todo: random icon
    P1Tab icon ->
      model
    P2Tab icon ->
      model
    P1Ready ->
      case model.state of
        Pause _ p2Ready -> { model | state = Pause True p2Ready }
        _ -> model
    P2Ready ->
      case model.state of
        Pause p1Ready _ -> { model | state = Pause p1Ready True }
        _ -> model
    -- todo: running wenn both players are ready

iconClick : Signal.Mailbox (Int, Int)
iconClick = Signal.mailbox (0, 0)

-- VIEW

{-| The game field extends from -1000 to +1000 in x and y coordinates. -}
(gameWidth,gameHeight) = (2400,1200)

view : Model -> Form
view model =
  let
    forms =
      [ show model |> toForm, drawHints model.hintIcons ] ++
      List.map (move (-1050, 0)) (drawIcons model.icons 0) ++
      List.map (move ( 1050, 0)) (drawIcons model.icons 1)
  in
    forms |> group

drawHints : (Icon, Icon) -> Form
drawHints (hint0, hint1) =
  group [
      drawIcon hint0 False |> rotate (degrees 90) |> move (0, -250)
    , drawIcon hint1 False |> rotate (degrees 90) |> move (0, 250)
  ]

drawIcons : List Icon -> Int -> List Form
drawIcons icons playerNum =
  let
    nthIcon n = drawClickableIcon (nthElement n icons) playerNum n
                |> move (0, 460 - 230 * (toFloat n))
  in
    List.map nthIcon [0..4]

iconSize : Int
iconSize = 210

drawClickableIcon : Icon -> Int -> Int -> Form
drawClickableIcon icon playerNum iconNum =
  let
    drawnForm = drawIcon icon (playerNum > 0)
    elem = collage iconSize iconSize [drawnForm]
  in
    elem
    |> Graphics.Input.clickable
      (Signal.message iconClick.address (playerNum, iconNum))
    |> toForm

drawIconBorder : Form
drawIconBorder =
  let
    width = 8
    lsGray = solid Color.charcoal
    grayLSWide = { lsGray | width = width, join = Smooth, cap = Round }
  in
    rect (toFloat iconSize) (toFloat iconSize) |> outlined grayLSWide

drawIcon : Icon -> Bool -> Form
drawIcon {color, shape} rot180 =
  let
    angle = if rot180 then 180 else 0
  in
    group [ drawIconBorder
          , toRGB color |> toDrawFunction shape |> rotate (degrees angle) ]

toRGB : Color -> Color.Color
toRGB color =
  case color of
    Yellow -> Color.yellow
    Blue -> Color.blue
    Orange -> Color.orange
    Green -> Color.green
    Red -> Color.red

toDrawFunction : Shape -> (Color.Color -> Form)
toDrawFunction shape =
  case shape of
    Circle -> drawCircle
    X -> drawX
    Square -> drawSquare
    Triangle -> drawTriangle
    Star -> drawStar

drawSquare : Color.Color -> Form
drawSquare color = filled color (Graphics.Collage.square 200)

drawCircle : Color.Color -> Form
drawCircle color = filled color (Graphics.Collage.circle 100)

drawX : Color.Color -> Form
drawX color =
  let
    points = [
        (-100, 70)
      , (-30, 0)
      , (-100, -70)
      , (-70, -100)
      , (0, -30)
      , (70, -100)
      , (100, -70)
      , (30, 0)
      , (100, 70)
      , (70, 100)
      , (0, 30)
      , (-70, 100)
      ]
  in
    filled color (Graphics.Collage.polygon points)

drawTriangle : Color.Color -> Form
drawTriangle color =
  let
    points = [
        (-100, 100)
      , (-100, -100)
      , (100, 0)
      ]
  in
    filled color (Graphics.Collage.polygon points)

-- http://mathworld.wolfram.com/Pentagon.html
drawStar : Color.Color -> Form
drawStar color =
  let
    r = 200 / (1 + cos (pi / 5))
    dx = r - 100
    c1 = cos (2*pi / 5) * r
    c2 = cos (pi / 5) * r
    s1 = sin (2*pi / 5) * r
    s2 = sin (4*pi / 5) * r
    points = [
        (-c2 - dx, s2)
      , (r - dx, 0)
      , (-c2 - dx, -s2)
      , (c1 - dx, s1)
      , (c1 - dx, -s1)
      ]
  in
    filled color (Graphics.Collage.polygon points)

actionSig : Signal Action
actionSig = Signal.sampleOn iconClick.signal (Signal.constant Start)

input : Signal Input
input = Signal.map2 Input actionSig randomSeed


-- MAIN

randomSeed : Signal.Signal Random.Seed
randomSeed = Signal.map (round >> Random.initialSeed) <| Signal.sampleOn actionSig (Time.every Time.second)

gameState : Signal.Signal Model
gameState = Signal.foldp update introModel input

gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW,gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

{-| Draw game maximized into the window. -}
displayFullScreen : (Int,Int) -> Model -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
    (centerX, centerY) = (gameWidth // 2, gameHeight // 2)
  in
    collage w h [ rect (toFloat w) (toFloat h) |> filled Color.darkCharcoal
                , view game |> scale factor ]

main = Signal.map2 displayFullScreen windowDimensions gameState

-- todo: up to 9 points