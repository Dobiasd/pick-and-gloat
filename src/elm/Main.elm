module Main where

import Color
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input
import Random
import Text
import Time

port windowWidth : Signal Int
port windowHeight : Signal Int

port windowDimensions : Signal (Int, Int)
port windowDimensions = Signal.map2 (\x y -> (x, y)) windowWidth windowHeight

-- MODEL

type Color = Yellow | Blue | Orange | Green | Red
--             | Cyan | Magenta | Brown
type Shape = Circle | X | Square | Triangle | Star
--             | Pentagon | Crescent | Heart | Arrow | Checkmark

type alias Icon = {
    color : Color
  , shape : Shape
  }

allPossibleColors : List Color
allPossibleColors = [ Yellow, Blue, Orange, Green, Red ]

allPossibleShapes : List Shape
allPossibleShapes = [ Circle, X, Square, Triangle, Star ]

-- Pause p1ready p2ready lastPointTo whoTapped whatWasTapped
-- Done p1ready p2ready lastPointTo whoTapped whatWasTapped
type State = Intro | Pause Bool Bool Int Int Int | Running | Done Bool Bool Int Int Int

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

type Action = IntroClose | P1Tab Int | P2Tab Int | P1Ready | P2Ready

type alias Input = { action : Action, seed : Random.Seed }

update : Input -> Model -> Model
update {action, seed} model =
  let
    (newIcon, newHints, seed') = newIconWithHints model.icons seed
    (brandNewIcons, seed'') = createNewIcons seed'
    (brandNewIcon, brandNewHints, _) = newIconWithHints brandNewIcons seed''
  in
    case action of
      IntroClose ->
        { state = Pause False False 0 0 0
        , icons = brandNewIcons
        , icon = brandNewIcon
        , hintIcons = brandNewHints
        , points1 = 0
        , points2 = 0 }
      P1Tab iconNum ->
        let
          correct = nthElement iconNum model.icons == model.icon
          pointTo = if correct then 1 else 2
          points1' = model.points1 + (if correct then 1 else 0)
          points2' = model.points2 + (if correct then 0 else 1)
          state' = if points1' >= maxPoints || points2' >= maxPoints
            then Done False False pointTo 1 iconNum
            else
              if pointTo == 1 then Pause False True pointTo 1 iconNum
                else Pause True False pointTo 1 iconNum
        in
          { model | state = state'
                  , points1 = points1'
                  , points2 = points2' }
      P2Tab iconNum ->
        let
          correct = nthElement iconNum model.icons == model.icon
          pointTo = if correct then 2 else 1
          points1' = model.points1 + (if correct then 0 else 1)
          points2' = model.points2 + (if correct then 1 else 0)
          state' = if points1' >= maxPoints || points2' >= maxPoints
            then Done False False pointTo 2 iconNum
            else
              if pointTo == 1 then Pause False True pointTo 2 iconNum
                else Pause True False pointTo 2 iconNum
        in
          { model | state = state'
                  , points1 = points1'
                  , points2 = points2' }
      P1Ready ->
        case model.state of
          Pause _ p2Ready lastPointTo whoTapped whatWasTapped ->
            if p2Ready then
              { model | state = Running
              , icon = newIcon
              , hintIcons = newHints }
            else
              { model | state = Pause True False lastPointTo whoTapped whatWasTapped }
          Done _ p2Ready lastPointTo whoTapped whatWasTapped ->
            if p2Ready then
              { model | state = Running
              , icons = brandNewIcons
              , icon = brandNewIcon
              , points1 = 0
              , points2 = 0
              , hintIcons = brandNewHints }
            else
              { model | state = Done True False lastPointTo whoTapped whatWasTapped }
          _ -> model
      P2Ready ->
        case model.state of
          Pause p1Ready _ lastPointTo whoTapped whatWasTapped ->
            if p1Ready then
              { model | state = Running
              , icon = newIcon
              , hintIcons = newHints }
            else
              { model | state = Pause False True lastPointTo whoTapped whatWasTapped }
          Done p1Ready _ lastPointTo whoTapped whatWasTapped ->
            if p1Ready then
              { model | state = Running
              , icons = brandNewIcons
              , icon = brandNewIcon
              , points1 = 0
              , points2 = 0
              , hintIcons = brandNewHints }
            else
              { model | state = Done False True lastPointTo whoTapped whatWasTapped }
          _ -> model

gameIconClick : Signal.Mailbox (Int, Int)
gameIconClick = Signal.mailbox (0, 0)

p1ReadyIconClick : Signal.Mailbox ()
p1ReadyIconClick = Signal.mailbox ()

p2ReadyIconClick : Signal.Mailbox ()
p2ReadyIconClick = Signal.mailbox ()

introCloseClick : Signal.Mailbox ()
introCloseClick = Signal.mailbox ()

-- VIEW

{-| The game field extends from -1000 to +1000 in x and y coordinates. -}
(gameWidth,gameHeight) = (2400,1200)

view : Model -> Form
view model =
  case model.state of
    Intro -> viewIntro model
    Pause _ _ _ _ _ -> viewPause model
    Running -> viewRunning model
    Done _ _ _ _ _ -> viewDone model

viewIntro : Model -> Form
viewIntro model =
  group [
      rect 1200 340
      |> filled Color.lightGray
    ,
      "Tab to start.\n\nCompete with a friend at one smartphone/tablet face to face.\nYou will be shown two hint icons in the middle that exclude colors and shapes.\nTab the one icon not excluded by the hints on your side.\nThe quicker player scores."
      |> toColoredSizedText Color.darkCharcoal 32
  ]
  |> singletonList
  |> collage 1200 340
  |> Graphics.Input.clickable
      (Signal.message introCloseClick.address ())
  |> toForm

viewPauseOrDone : Model -> Form
viewPauseOrDone model =
  let
    (isDone, p1Ready, p2Ready, lastPointTo, whoTapped, whatWasTapped) =
    case model.state of
      Pause p1Ready p2Ready lastPointTo whoTapped whatWasTapped -> (False, p1Ready, p2Ready, lastPointTo, whoTapped, whatWasTapped)
      Done p1Ready p2Ready lastPointTo whoTapped whatWasTapped -> (True, p1Ready, p2Ready, lastPointTo, whoTapped, whatWasTapped)
      otherwise -> Debug.crash "viewPauseOrDone"
    -- changing viewReadyButton and its calls can lead to the elements not being clickable any more
    viewReadyButton mailbox col =
      rect 400 100 |> filled col
          |> \x -> [x, toColoredSizedText Color.darkCharcoal 48 "Tab when ready"]
          |> collage 400 100
          |> Graphics.Input.clickable
            (Signal.message mailbox.address ())
          |> toForm
    readyIcon1 =
      viewReadyButton p1ReadyIconClick
        (if p1Ready then Color.darkCharcoal else Color.yellow)
      |> rotate (degrees  90)
    readyIcon2 =
      viewReadyButton p2ReadyIconClick
        (if p2Ready then Color.darkCharcoal else Color.yellow)
      |> rotate (degrees -90)
    tapWasCorrect = lastPointTo == whoTapped
    tabIconBorderColor = if tapWasCorrect then Color.green else Color.red
    tapBorder = drawIconBorder 24 tabIconBorderColor
      |> moveX (gameIconOffsetX * (if whoTapped == 2 then 1 else -1))
      |> moveY (iconPosY whatWasTapped)
    (winText, looseText) =
      if isDone then
        ( toColoredSizedText Color.green 204 "You win!"
        , toColoredSizedText Color.red 204 "You loose!" )
      else
        ( toColoredSizedText Color.green 96 "You score!"
        , toColoredSizedText Color.red 96 "" )
    (p1TextRaw, p2TextRaw) = if lastPointTo == 1 then (winText, looseText) else (looseText, winText)
    p1Text = p1TextRaw |> rotate (degrees -90) |> moveX -500
    p2Text = p2TextRaw |> rotate (degrees  90) |> moveX  500
  in
    viewGameIcons model ++
    [ if whoTapped < 1 then dummyForm else tapBorder
    , viewPoints model
    , if whoTapped < 1 then dummyForm else drawHints model.hintIcons
    , if whoTapped < 1 then dummyForm else p1Text
    , if whoTapped < 1 then dummyForm else p2Text
    , readyIcon1 |> moveX  740
    , readyIcon2 |> moveX -740 ]
    |> group

dummyForm : Form
dummyForm = rect 0 0 |> filled Color.darkCharcoal

maxPoints : Int
maxPoints = 5

viewPoints : Model -> Form
viewPoints model =
  let
    circleDist = 64
    viewPointRow score =
      List.map (\n -> Graphics.Collage.circle 24
            |> filled
              (if n <= score then Color.lightGreen else Color.darkGray)
            |> moveY ((toFloat (-maxPoints // 2) * circleDist) + toFloat (n - 1) * circleDist))
        [1..maxPoints]
    row1 = viewPointRow model.points1 |> List.map (rotate (degrees -90) >> moveX -286)
    row2 = viewPointRow model.points2 |> List.map (rotate (degrees  90) >> moveX  286)
  in
    row1 ++ row2 |> group

viewRunning : Model -> Form
viewRunning model =
  let
    forms =
      [ drawHints model.hintIcons
      , viewPoints model ] ++
      viewGameIcons model
  in
    forms |> group

gameIconOffsetX : Float
gameIconOffsetX = 1050

viewGameIcons : Model -> List Form
viewGameIcons model =
      List.map (moveX -gameIconOffsetX) (drawIcons model.icons 0) ++
      List.map (moveX  gameIconOffsetX) (drawIcons model.icons 1)

viewDone : Model -> Form
viewDone model =
  viewPauseOrDone model

viewPause : Model -> Form
viewPause model =
  viewPauseOrDone model

drawHints : (Icon, Icon) -> Form
drawHints (hint0, hint1) =
  group [
      drawIcon hint0 |> move (0, -250)
    , drawIcon hint1 |> move (0, 250)
  ]

iconPosY : Int -> Float
iconPosY n = 460 - 230 * (toFloat n)

drawIcons : List Icon -> Int -> List Form
drawIcons icons playerNum =
  let
    nthIcon n = drawClickableIcon (nthElement n icons) playerNum n
                |> moveY (iconPosY n)
  in
    List.map nthIcon [0..4]

iconSize : Int
iconSize = 210

drawClickableIcon : Icon -> Int -> Int -> Form
drawClickableIcon icon playerNum iconNum =
  let
    drawnForm = drawIcon icon
    elem = collage iconSize iconSize [drawnForm]
  in
    elem
    |> Graphics.Input.clickable
      (Signal.message gameIconClick.address (playerNum, iconNum))
    |> toForm

drawIconBorder : Float -> Color.Color -> Form
drawIconBorder w color =
  let
    lsGray = solid color
    grayLSWide = { lsGray | width = w, join = Smooth, cap = Round }
  in
    rect (toFloat iconSize) (toFloat iconSize) |> outlined grayLSWide

drawIcon : Icon -> Form
drawIcon {color, shape} =
  group [ drawIconBorder 8 Color.charcoal
        , toRGB color |> toDrawFunction shape ]

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
    |> rotate (degrees 90)

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
    |> rotate (degrees 90)

actionSigReady1Click : Signal Action
actionSigReady1Click = Signal.sampleOn p1ReadyIconClick.signal (Signal.constant P1Ready)

actionSigReady2Click : Signal Action
actionSigReady2Click = Signal.sampleOn p2ReadyIconClick.signal (Signal.constant P2Ready)

actionSigIntroCloseClick : Signal Action
actionSigIntroCloseClick = Signal.sampleOn introCloseClick.signal (Signal.constant <| IntroClose)

actionSigIconClick : Signal Action
actionSigIconClick =
  let
    f (playerNum, iconNum ) =
      if playerNum == 0 then
        P1Tab iconNum
      else
        P2Tab iconNum
  in
    Signal.map f gameIconClick.signal

actionSig =
  Signal.mergeMany [
      actionSigIntroCloseClick
    , actionSigReady1Click
    , actionSigReady2Click
    , actionSigIconClick ]

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

singletonList : a -> List a
singletonList x = [x]

toColoredSizedText : Color.Color -> Float -> String -> Form
toColoredSizedText col s = Text.fromString >> Text.height s >> Text.color col
                           >> leftAligned >> toForm

viewCopyright : Int -> Form
viewCopyright winHeight =
  toColoredSizedText Color.charcoal 14
    "Copyright © 2016 Tobias Hermann. All rights reserved."
  |> moveY ((toFloat -winHeight / 2) + 24)

{-| Draw game maximized into the window. -}
displayFullScreen : (Int,Int) -> Model -> Element
displayFullScreen (w,hWithoutAds) game =
  let
    addHeight = 90
    h = hWithoutAds - addHeight
    factor = gameScale (w,h) (gameWidth,gameHeight)
    (centerX, centerY) = (gameWidth // 2, gameHeight // 2)
  in
    flow down [
      rect (toFloat w) (toFloat addHeight)
        |> filled Color.darkCharcoal
        |> singletonList
        |> collage w addHeight
      , collage w h [ rect (toFloat w) (toFloat h)
         |> filled Color.darkCharcoal
         , viewCopyright h
         , view game |> scale factor ] ]

main = Signal.map2 displayFullScreen windowDimensions gameState

-- todo: besserer name fuers spiel
-- todo: als android-app: http://developer.android.com/guide/webapps/index.html
-- todo: hard mode mit mehr dingern
-- todo: andere shapes verwenden, damit es nicht zu gleich ist
-- todo: green check mark oder red x anzeigen wenn einer gedrueckt hat
-- todo: tap-on-down benutzen, auch bei demoscene