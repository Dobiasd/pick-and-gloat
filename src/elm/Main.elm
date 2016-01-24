module Main (..) where

import Color
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input
import Random
import Text
import Time
import Html
import Html.Events
import Json.Decode


port windowWidth : Signal Int
port windowHeight : Signal Int
port windowDimensions : Signal ( Int, Int )
port windowDimensions =
    Signal.map2 (\x y -> ( x, y )) windowWidth windowHeight



-- MODEL


type Color
    = Yellow
    | Blue
    | Orange
    | Green
    | Red



--             | Cyan | Magenta | Brown


type Shape
    = Circle
    | X
    | Square
    | Triangle
    | Star



--             | Pentagon | Crescent | Heart | Arrow | Checkmark


type alias Icon =
    { color : Color
    , shape : Shape
    }


allPossibleColors : List Color
allPossibleColors =
    [ Yellow, Blue, Orange, Green, Red ]


allPossibleShapes : List Shape
allPossibleShapes =
    [ Circle, X, Square, Triangle, Star ]



-- Pause p1ready p2ready lastPointTo whoTapped whatWasTapped
-- Done p1ready p2ready lastPointTo whoTapped whatWasTapped


type State
    = Intro
    | Pause Bool Bool Int Int Int
    | Running
    | Done Bool Bool Int Int Int


type alias Model =
    { state : State
    , icons : List Icon
    , icon : Icon
    , hintIcons : ( Icon, Icon )
    , points1 : Int
    , points2 : Int
    }


{-| splitAt 2 [1,2,3,4,5,6] === ([1,2],[3,4,5,6])
-}
splitAt : Int -> List a -> ( List a, List a )
splitAt n l =
    case ( n, l ) of
        ( 0, xs ) ->
            ( [], xs )

        ( _, [] ) ->
            ( [], [] )

        ( n, x :: xs ) ->
            let
                ( xs', xs'' ) = splitAt (n - 1) xs
            in
                ( x :: xs', xs'' )


init : List a -> List a
init l =
    case l of
        x :: [] ->
            []

        x :: xs ->
            x :: init xs

        _ ->
            Debug.crash "init failed"


nthElement : Int -> List a -> a
nthElement n =
    List.drop n >> unsafeHead


unsafeHead : List a -> a
unsafeHead xs =
    case xs of
        x :: _ ->
            x

        _ ->
            Debug.crash "unsafeHead with empty list"


{-| Get last element of a list. (not total)
-}
last : List a -> a
last =
    List.reverse >> unsafeHead


{-| Suffle a list using random numbers
The length of the list of random numbers has to be larger
as long as the list that shall be shuffled.
-}
shuffleHelper : List Int -> List a -> List a
shuffleHelper randoms l =
    case randoms of
        i :: is ->
            case l of
                x :: xs ->
                    let
                        ( firsts, rest ) = splitAt (i % List.length l + 1) l
                    in
                        (last firsts) :: shuffleHelper is (init firsts ++ rest)

                x ->
                    x

        _ ->
            Debug.crash "shuffle failed"


shuffle : Random.Seed -> List a -> ( List a, Random.Seed )
shuffle seed xs =
    let
        intGenerator = Random.int Random.minInt Random.maxInt

        listGenerator = Random.list (List.length xs + 1) intGenerator

        ( randomNumbers, seed' ) = Random.generate listGenerator seed
    in
        ( shuffleHelper randomNumbers xs, seed' )


createNewIcons : Random.Seed -> ( List Icon, Random.Seed )
createNewIcons seed0 =
    let
        ( colors, seed1 ) = shuffle seed0 allPossibleColors

        ( shapes, seed2 ) = shuffle seed1 allPossibleShapes
    in
        ( List.map2 Icon colors shapes, seed2 )


newIconWithHints : List Icon -> Random.Seed -> ( Icon, ( Icon, Icon ), Random.Seed )
newIconWithHints icons seed0 =
    let
        ( shuffledIcons, seed1 ) = shuffle seed0 icons

        icon = nthElement 0 shuffledIcons

        remainingIcon0 = nthElement 1 shuffledIcons

        remainingIcon1 = nthElement 2 shuffledIcons

        remainingIcon2 = nthElement 3 shuffledIcons

        remainingIcon3 = nthElement 4 shuffledIcons

        hint0 = Icon remainingIcon0.color remainingIcon2.shape

        hint1 = Icon remainingIcon1.color remainingIcon3.shape
    in
        ( icon, ( hint0, hint1 ), seed1 )


introModel : Model
introModel =
    { state = Intro
    , icons =
        [ Icon Blue X
        , Icon Orange Square
        , Icon Yellow Circle
        , Icon Green Triangle
        , Icon Red Star
        ]
    , icon = Icon Yellow Circle
    , hintIcons = ( Icon Green Star, Icon Blue Square )
    , points1 = 0
    , points2 = 0
    }



-- UPDATE


type Action
    = IntroClose
    | P1Tab Int
    | P2Tab Int
    | P1Ready
    | P2Ready


type alias Input =
    { action : Action, seed : Random.Seed }


update : Input -> Model -> Model
update { action, seed } model =
    let
        ( newIcon, newHints, seed' ) = newIconWithHints model.icons seed

        ( brandNewIcons, seed'' ) = createNewIcons seed'

        ( brandNewIcon, brandNewHints, _ ) = newIconWithHints brandNewIcons seed''
    in
        case action of
            IntroClose ->
                { state = Pause False False 0 0 0
                , icons = brandNewIcons
                , icon = brandNewIcon
                , hintIcons = brandNewHints
                , points1 = 0
                , points2 = 0
                }

            P1Tab iconNum ->
                let
                    correct = nthElement iconNum model.icons == model.icon

                    pointTo =
                        if correct then
                            1
                        else
                            2

                    points1' =
                        model.points1
                            + (if correct then
                                1
                               else
                                0
                              )

                    points2' =
                        model.points2
                            + (if correct then
                                0
                               else
                                1
                              )

                    state' =
                        if points1' >= maxPoints || points2' >= maxPoints then
                            Done False False pointTo 1 iconNum
                        else if pointTo == 1 then
                            Pause False True pointTo 1 iconNum
                        else
                            Pause True False pointTo 1 iconNum
                in
                    if model.state == Running then
                        { model
                            | state = state'
                            , points1 = points1'
                            , points2 = points2'
                        }
                    else
                        model

            P2Tab iconNum ->
                let
                    correct = nthElement iconNum model.icons == model.icon

                    pointTo =
                        if correct then
                            2
                        else
                            1

                    points1' =
                        model.points1
                            + (if correct then
                                0
                               else
                                1
                              )

                    points2' =
                        model.points2
                            + (if correct then
                                1
                               else
                                0
                              )

                    state' =
                        if points1' >= maxPoints || points2' >= maxPoints then
                            Done False False pointTo 2 iconNum
                        else if pointTo == 1 then
                            Pause False True pointTo 2 iconNum
                        else
                            Pause True False pointTo 2 iconNum
                in
                    if model.state == Running then
                        { model
                            | state = state'
                            , points1 = points1'
                            , points2 = points2'
                        }
                    else
                        model

            P1Ready ->
                case model.state of
                    Pause _ p2Ready lastPointTo whoTapped whatWasTapped ->
                        if p2Ready then
                            { model
                                | state = Running
                                , icon = newIcon
                                , hintIcons = newHints
                            }
                        else
                            { model | state = Pause True False lastPointTo whoTapped whatWasTapped }

                    Done _ p2Ready lastPointTo whoTapped whatWasTapped ->
                        if p2Ready then
                            { model
                                | state = Running
                                , icons = brandNewIcons
                                , icon = brandNewIcon
                                , points1 = 0
                                , points2 = 0
                                , hintIcons = brandNewHints
                            }
                        else
                            { model | state = Done True False lastPointTo whoTapped whatWasTapped }

                    _ ->
                        model

            P2Ready ->
                case model.state of
                    Pause p1Ready _ lastPointTo whoTapped whatWasTapped ->
                        if p1Ready then
                            { model
                                | state = Running
                                , icon = newIcon
                                , hintIcons = newHints
                            }
                        else
                            { model | state = Pause False True lastPointTo whoTapped whatWasTapped }

                    Done p1Ready _ lastPointTo whoTapped whatWasTapped ->
                        if p1Ready then
                            { model
                                | state = Running
                                , icons = brandNewIcons
                                , icon = brandNewIcon
                                , points1 = 0
                                , points2 = 0
                                , hintIcons = brandNewHints
                            }
                        else
                            { model | state = Done False True lastPointTo whoTapped whatWasTapped }

                    _ ->
                        model


gameIconClick : Signal.Mailbox ( Int, Int )
gameIconClick =
    Signal.mailbox ( 0, 0 )


p1ReadyIconClick : Signal.Mailbox ()
p1ReadyIconClick =
    Signal.mailbox ()


p2ReadyIconClick : Signal.Mailbox ()
p2ReadyIconClick =
    Signal.mailbox ()


introCloseClick : Signal.Mailbox ()
introCloseClick =
    Signal.mailbox ()



-- VIEW
-- https://github.com/igrep/elm-touchmove-mousemove-sample


onBoth : List String -> Signal.Address a -> a -> List Html.Attribute
onBoth events a x =
    List.map (\event -> messageOn event a x) events



-- NOTE: Copied from Html.Events. Why isn't it exported...?


messageOn : String -> Signal.Address a -> a -> Html.Attribute
messageOn name addr msg =
    Html.Events.on name Json.Decode.value (\_ -> Signal.message addr msg)



-- https://groups.google.com/forum/#!topic/elm-discuss/eiOIhAi24wk


tappable : Signal.Address a -> a -> Graphics.Element.Element -> Graphics.Element.Element
tappable address msg elem =
    Html.div
        --player gets two points if we use onBoth with both events.
        --[ Html.Events.onMouseDown address msg ]
        --[ Html.Events.on "touchmove" Json.Decode.value (\_ -> Signal.message address msg) ]
        (onBoth [ "mousedown", "touchstart" ] address msg)
        [ Html.fromElement elem ]
        |> Html.toElement
            (Graphics.Element.widthOf elem)
            (Graphics.Element.heightOf elem)


( gameWidth, gameHeight ) =
    ( 2400, 1200 )


view : Float -> Model -> Form
view scale model =
    case model.state of
        Intro ->
            viewIntro scale model

        Pause _ _ _ _ _ ->
            viewPause scale model

        Running ->
            viewRunning True scale model

        Done _ _ _ _ _ ->
            viewDone scale model


viewIntro : Float -> Model -> Form
viewIntro scale model =
    group
        [ rect (scale * toFloat gameWidth) (scale * 470)
            |> filled Color.lightGray
        , "Tab to start"
            |> toColoredSizedText Color.darkCharcoal (scale * 120)
            |> moveX (scale * -230)
        , viewExplanation scale model
        ]
        |> singletonList
        |> collage
            (scale * toFloat gameWidth |> round)
            (scale * toFloat gameHeight |> round)
        |> tappable introCloseClick.address ()
        |> toForm


viewExplanation : Float -> Model -> Form
viewExplanation scale model =
    group
        [ viewSmartphone (1.5 * scale) model
        , viewPerson (1.5 * scale) |> moveY (scale * -430)
        , viewPerson (1.5 * scale) |> rotate (degrees 180) |> moveY (scale * 430)
        , group
            [ viewSmartphone (8.1 * scale) model
            , viewSmartphoneOverlay (8.1 * scale)
            ]
            |> move ( scale * 1340, scale * 430 )
        ]
        |> moveX (scale * -700)


viewSmartphone : Float -> Model -> Form
viewSmartphone scale model =
    group
        [ filled
            Color.black
            (Graphics.Collage.rect (scale * 130) (scale * 220))
        , filled
            Color.darkCharcoal
            (Graphics.Collage.rect (scale * 120) (scale * 210))
        , viewRunning False (scale * 8.5e-2) model
            |> rotate (degrees 90)
        ]


drawExcludeLine : Float -> ( Float, Float ) -> ( Float, Float ) -> Form
drawExcludeLine scale ( x1, y1 ) ( x2, y2 ) =
    let
        lsCol1 = solid Color.black

        lsFull1 = { lsCol1 | width = (scale * 3.0) }

        lsCol2 = solid Color.red

        lsFull2 = { lsCol2 | width = (scale * 2) }
    in
        group
            [ path [ ( scale * x1, scale * y1 ), ( scale * x2, scale * y2 ) ]
                |> traced lsFull1
            , path [ ( scale * x1, scale * y1 ), ( scale * x2, scale * y2 ) ]
                |> traced lsFull2
            ]


drawCheckMark : Float -> Form
drawCheckMark scale =
    let
        lsCol = solid Color.darkGreen

        lsFull = { lsCol | width = (scale * 3.0) }
    in
        path
            [ ( scale * -7, scale * 0 )
            , ( scale * 0, scale * -7 )
            , ( scale * 7, scale * 7 )
            ]
            |> traced lsFull


viewSmartphoneOverlay : Float -> Form
viewSmartphoneOverlay scale =
    let
        drawLine = drawExcludeLine scale
    in
        group
            [ drawLine ( -39, -90 ) ( -24, 0 )
            , drawLine ( -20, -90 ) ( -19, 0 )
            , drawLine ( 39, -90 ) ( 24, 0 )
            , drawLine ( 20, -90 ) ( 19, 0 )
            , drawCheckMark scale |> moveY (scale * -68)
            ]


viewPerson : Float -> Form
viewPerson scale =
    let
        head = filled Color.lightBlue (Graphics.Collage.circle (scale * 100))

        leftUpperArm =
            filled
                Color.lightBlue
                (Graphics.Collage.rect (scale * 200) (scale * 60))
                |> move ( scale * -140, scale * -14 )

        leftLowerArm =
            filled
                Color.lightBlue
                (Graphics.Collage.rect (scale * 60) (scale * 200))
                |> move ( scale * -210, scale * 60 )

        rightUpperArm =
            filled
                Color.lightBlue
                (Graphics.Collage.rect (scale * 200) (scale * 60))
                |> rotate (degrees 45)
                |> move ( scale * 140, scale * 70 )

        rightLowerArm =
            filled
                Color.lightBlue
                (Graphics.Collage.rect (scale * 60) (scale * 200))
                |> rotate (degrees 45)
                |> move ( scale * 140, scale * 170 )
    in
        group
            [ head
            , leftUpperArm
            , leftLowerArm
            , rightUpperArm
            , rightLowerArm
            ]


viewPauseOrDone : Float -> Model -> Form
viewPauseOrDone scale model =
    let
        ( isDone, p1Ready, p2Ready, lastPointTo, whoTapped, whatWasTapped ) =
            case model.state of
                Pause p1Ready p2Ready lastPointTo whoTapped whatWasTapped ->
                    ( False, p1Ready, p2Ready, lastPointTo, whoTapped, whatWasTapped )

                Done p1Ready p2Ready lastPointTo whoTapped whatWasTapped ->
                    ( True, p1Ready, p2Ready, lastPointTo, whoTapped, whatWasTapped )

                otherwise ->
                    Debug.crash "viewPauseOrDone"

        viewReadyButton mailbox col =
            let
                lsCol = solid col

                lsFull = { lsCol | width = (scale * 16) }
            in
                [ rect (scale * 610) (scale * 170) |> outlined lsFull
                , toColoredSizedText
                    col
                    (scale * 64)
                    (if isDone then
                        "Tab to restart"
                     else
                        "Tab when ready"
                    )
                ]
                    |> collage (scale * 610 |> round) (scale * 170 |> round)
                    |> tappable mailbox.address ()
                    |> toForm

        readyIcon1 =
            viewReadyButton
                p1ReadyIconClick
                (if p1Ready then
                    Color.darkCharcoal
                 else
                    Color.yellow
                )
                |> rotate (degrees 90)

        readyIcon2 =
            viewReadyButton
                p2ReadyIconClick
                (if p2Ready then
                    Color.darkCharcoal
                 else
                    Color.yellow
                )
                |> rotate (degrees -90)

        tapWasCorrect = lastPointTo == whoTapped

        tabIconBorderColor =
            if tapWasCorrect then
                Color.green
            else
                Color.red

        tapBorder =
            drawIconBorder scale 24 tabIconBorderColor
                |> moveX
                    (gameIconOffsetX scale
                        * (if whoTapped == 2 then
                            1
                           else
                            -1
                          )
                    )
                |> moveY (iconPosY scale whatWasTapped)

        ( winText, looseText ) =
            if isDone then
                ( group
                    [ rect (scale * 1060) (scale * 240) |> filled Color.white
                    , toColoredSizedText Color.green (scale * 230) "You win!"
                    ]
                , group
                    [ rect (scale * 1060) (scale * 240) |> filled Color.black
                    , toColoredSizedText Color.red (scale * 230) "You loose!"
                    ]
                )
            else
                ( toColoredSizedText
                    Color.green
                    (scale * 140)
                    (if lastPointTo == whoTapped then
                        "Correct! You Score."
                     else
                        "You score."
                    )
                , toColoredSizedText
                    Color.red
                    (scale * 140)
                    (if lastPointTo == whoTapped then
                        "Too slow!"
                     else
                        "Wrong!"
                    )
                )

        ( p1TextRaw, p2TextRaw ) =
            if lastPointTo == 1 then
                ( winText, looseText )
            else
                ( looseText, winText )

        p1Text = p1TextRaw |> rotate (degrees -90) |> moveX (scale * (-480))

        p2Text = p2TextRaw |> rotate (degrees 90) |> moveX (scale * 480)
    in
        viewGameIcons scale model
            ++ grayGameIconOverlay scale
            ++ [ if whoTapped < 1 then
                    dummyForm
                 else
                    tapBorder
               , viewPoints scale model
               , if whoTapped < 1 then
                    dummyForm
                 else
                    drawHints scale model.hintIcons
               , if whoTapped < 1 then
                    dummyForm
                 else
                    p1Text
               , if whoTapped < 1 then
                    dummyForm
                 else
                    p2Text
               , readyIcon1 |> moveX (scale * 740)
               , readyIcon2 |> moveX (scale * (-740))
               ]
            |> group


grayGameIconOverlay : Float -> List Form
grayGameIconOverlay scale =
    let
        overlayWidth = scale * 300

        offsetX = (scale * toFloat gameWidth) / 2 - overlayWidth / 2

        oneBar =
            rect overlayWidth gameHeight
                |> filled (Color.rgba 127 127 127 0.5)
    in
        [ oneBar |> moveX offsetX
        , oneBar |> moveX -offsetX
        ]


dummyForm : Form
dummyForm =
    rect 0 0 |> filled Color.darkCharcoal


maxPoints : Int
maxPoints =
    5


viewPoints : Float -> Model -> Form
viewPoints scale model =
    let
        circleDist = scale * 64

        viewPointRow score =
            List.map
                (\n ->
                    Graphics.Collage.circle (scale * 24)
                        |> filled
                            (if n <= score then
                                Color.lightGreen
                             else
                                Color.darkGray
                            )
                        |> moveY ((toFloat (-maxPoints // 2) * circleDist) + toFloat (n - 1) * circleDist)
                )
                [1..maxPoints]

        row1 = viewPointRow model.points1 |> List.map (rotate (degrees -90) >> moveX (scale * (-286)))

        row2 = viewPointRow model.points2 |> List.map (rotate (degrees 90) >> moveX (scale * 286))
    in
        row1 ++ row2 |> group


viewRunning : Bool -> Float -> Model -> Form
viewRunning showPoints scale model =
    let
        forms =
            [ drawHints scale model.hintIcons
            , if showPoints then
                viewPoints scale model
              else
                dummyForm
            ]
                ++ viewGameIcons scale model
    in
        forms |> group


gameIconOffsetX : Float -> Float
gameIconOffsetX scale =
    scale * 1050


viewGameIcons : Float -> Model -> List Form
viewGameIcons scale model =
    List.map
        (moveX (-(gameIconOffsetX scale)))
        (drawIcons -90 scale model.icons 0)
        ++ List.map
            (moveX (gameIconOffsetX scale))
            (drawIcons 90 scale model.icons 1)


viewDone : Float -> Model -> Form
viewDone scale model =
    viewPauseOrDone scale model


viewPause : Float -> Model -> Form
viewPause scale model =
    viewPauseOrDone scale model


drawHints : Float -> ( Icon, Icon ) -> Form
drawHints scale ( hint0, hint1 ) =
    group
        [ drawIcon scale hint0 |> move ( 0, scale * (-250) )
        , drawIcon scale hint1 |> move ( 0, scale * 250 )
        ]


iconPosY : Float -> Int -> Float
iconPosY scale n =
    (scale * 460) - (scale * 230) * (toFloat n)


drawIcons : Float -> Float -> List Icon -> Int -> List Form
drawIcons rot scale icons playerNum =
    let
        nthIcon n =
            drawClickableIcon scale (nthElement n icons) playerNum n
                |> moveY (iconPosY scale n)
                |> rotate (degrees rot)
    in
        List.map nthIcon [0..4]


iconSize : Float -> Int
iconSize scale =
    scale * 210 |> round


drawClickableIcon : Float -> Icon -> Int -> Int -> Form
drawClickableIcon scale icon playerNum iconNum =
    let
        drawnForm = drawIcon scale icon

        elem = collage (iconSize scale) (iconSize scale) [ drawnForm ]
    in
        elem
            |> tappable gameIconClick.address ( playerNum, iconNum )
            |> toForm


drawIconBorder : Float -> Float -> Color.Color -> Form
drawIconBorder scale w color =
    let
        lsGray = solid color

        grayLSWide =
            { lsGray
                | width = (scale * w)
            }
    in
        rect (toFloat (iconSize scale)) (toFloat (iconSize scale)) |> outlined grayLSWide


drawIcon : Float -> Icon -> Form
drawIcon scale { color, shape } =
    group
        [ drawIconBorder scale 8 Color.charcoal
        , toRGB color |> toDrawFunction scale shape
        ]


toRGB : Color -> Color.Color
toRGB color =
    case color of
        Yellow ->
            Color.yellow

        Blue ->
            Color.blue

        Orange ->
            Color.orange

        Green ->
            Color.green

        Red ->
            Color.red


toDrawFunction : Float -> Shape -> Color.Color -> Form
toDrawFunction scale shape =
    case shape of
        Circle ->
            drawCircle scale

        X ->
            drawX scale

        Square ->
            drawSquare scale

        Triangle ->
            drawTriangle scale

        Star ->
            drawStar scale


drawSquare : Float -> Color.Color -> Form
drawSquare scale color =
    filled color (Graphics.Collage.square (scale * 200))


drawCircle : Float -> Color.Color -> Form
drawCircle scale color =
    filled color (Graphics.Collage.circle (scale * 100))


scalePoints : Float -> List ( Float, Float ) -> List ( Float, Float )
scalePoints scale =
    List.map (\( x, y ) -> ( scale * x, scale * y ))


drawX : Float -> Color.Color -> Form
drawX scale color =
    let
        points =
            [ ( -100, 70 )
            , ( -30, 0 )
            , ( -100, -70 )
            , ( -70, -100 )
            , ( 0, -30 )
            , ( 70, -100 )
            , ( 100, -70 )
            , ( 30, 0 )
            , ( 100, 70 )
            , ( 70, 100 )
            , ( 0, 30 )
            , ( -70, 100 )
            ]
                |> scalePoints scale
    in
        filled color (Graphics.Collage.polygon points)


drawTriangle : Float -> Color.Color -> Form
drawTriangle scale color =
    let
        points =
            [ ( -100, 100 )
            , ( -100, -100 )
            , ( 100, 0 )
            ]
                |> scalePoints scale
    in
        filled color (Graphics.Collage.polygon points)
            |> rotate (degrees 90)



-- http://mathworld.wolfram.com/Pentagon.html


drawStar : Float -> Color.Color -> Form
drawStar scale color =
    let
        r = (scale * 200) / (1 + cos (pi / 5))

        dx = r - (scale * 100)

        c1 = cos (2 * pi / 5) * r

        c2 = cos (pi / 5) * r

        s1 = sin (2 * pi / 5) * r

        s2 = sin (4 * pi / 5) * r

        points =
            [ ( -c2 - dx, s2 )
            , ( r - dx, 0 )
            , ( -c2 - dx, -s2 )
            , ( c1 - dx, s1 )
            , ( c1 - dx, -s1 )
            ]
    in
        filled color (Graphics.Collage.polygon points)
            |> rotate (degrees 90)


actionSigReady1Click : Signal Action
actionSigReady1Click =
    Signal.sampleOn p1ReadyIconClick.signal (Signal.constant P1Ready)


actionSigReady2Click : Signal Action
actionSigReady2Click =
    Signal.sampleOn p2ReadyIconClick.signal (Signal.constant P2Ready)


actionSigIntroCloseClick : Signal Action
actionSigIntroCloseClick =
    Signal.sampleOn introCloseClick.signal (Signal.constant <| IntroClose)


actionSigIconClick : Signal Action
actionSigIconClick =
    let
        f ( playerNum, iconNum ) =
            if playerNum == 0 then
                P1Tab iconNum
            else
                P2Tab iconNum
    in
        Signal.map f gameIconClick.signal


actionSig =
    Signal.mergeMany
        [ actionSigIntroCloseClick
        , actionSigReady1Click
        , actionSigReady2Click
        , actionSigIconClick
        ]


input : Signal Input
input =
    Signal.map2 Input actionSig randomSeed



-- MAIN


randomSeed : Signal.Signal Random.Seed
randomSeed =
    Signal.map (round >> Random.initialSeed) <| Signal.sampleOn actionSig (Time.every Time.second)


gameState : Signal.Signal Model
gameState =
    Signal.foldp update introModel input


gameScale : ( Int, Int ) -> ( Float, Float ) -> Float
gameScale ( winW, winH ) ( gameW, gameH ) =
    min (toFloat winW / gameW) (toFloat winH / gameH)


singletonList : a -> List a
singletonList x =
    [ x ]


toColoredSizedText : Color.Color -> Float -> String -> Form
toColoredSizedText col s =
    Text.fromString
        >> Text.height s
        >> Text.color col
        >> leftAligned
        >> toForm


viewCopyright : Float -> Form
viewCopyright scale =
    toColoredSizedText
        Color.charcoal
        (scale * 32)
        "Copyright © 2016 Tobias Hermann. All rights reserved."
        |> moveY (scale * -550)


{-| Draw game maximized into the window.
-}
displayFullScreen : ( Int, Int ) -> Model -> Element
displayFullScreen ( w, hWithoutAds ) game =
    let
        addHeight = 90

        h = hWithoutAds - addHeight

        factor = gameScale ( w, h ) ( gameWidth, gameHeight )

        ( centerX, centerY ) = ( gameWidth // 2, gameHeight // 2 )
    in
        flow
            down
            [ rect (toFloat w) (toFloat addHeight)
                |> filled Color.darkCharcoal
                |> singletonList
                |> collage w addHeight
            , collage
                w
                h
                [ rect (toFloat w) (toFloat h)
                    |> filled Color.darkCharcoal
                , view factor game
                , viewCopyright factor
                ]
            ]


main =
    Signal.map2 displayFullScreen windowDimensions gameState



-- todo: als android-app: http://developer.android.com/guide/webapps/index.html
-- todo: hard mode mit mehr dingern
-- todo: andere shapes verwenden, damit es nicht zu gleich ist
