port module StandSit exposing (Model, Msg(..), Pose(..), TimerMode(..), TimerState(..), initialModel, main, padLeadingZero, update, view)

-- import Html.Events exposing (onClick)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (onClick)
import String exposing (padLeft)
import Time


type Pose
    = Stand
    | Neutral
    | Sit


type Msg
    = ClickedPose Pose
    | Tick Time.Posix
    | ClickedTimerModeToggle
    | ClickedTimerState TimerState


padLeadingZero : Int -> String
padLeadingZero value =
    padLeft 2 '0' (String.fromInt value)


modeBasedTime : Model -> Int
modeBasedTime model =
    case model.timerMode of
        Elapsed ->
            model.timeElapsed

        Remaining ->
            model.timeValue - model.timeElapsed


currentTimeText : Model -> Html Msg
currentTimeText model =
    let
        seconds =
            modeBasedTime model

        minutes =
            (seconds |> toFloat)
                / 60
                |> floor
                |> padLeadingZero

        remainingSeconds =
            seconds
                |> remainderBy 60
                |> padLeadingZero

        timeText =
            if seconds == 0 && model.timerState == Stopped then
                "--:--"

            else
                minutes ++ ":" ++ remainingSeconds
    in
    span [ Attr.id "timeText", css [ Css.batch [ textAlign center, fontFamily monospace, fontSize (Css.em 1.5) ] ] ] [ text timeText ]


buttonStyle : Style
buttonStyle =
    Css.batch
        [ displayFlex
        , justifyContent center
        , alignItems center
        , borderRadius (px 2)
        , backgroundColor (rgb 250 250 250)
        , color (rgb 51 52 53)
        , border3 (px 1) solid (rgb 51 52 53)
        , margin (px 2)
        , padding4 (px 5) (px 20) (px 5) (px 20)
        , fontFamily monospace
        , focus
            [ outline zero
            , position relative
            , after
                [ property "content" "''"
                , position absolute
                , top (px -2)
                , right (px -2)
                , bottom (px -2)
                , left (px -2)
                , borderRadius (px 2)
                , backgroundColor transparent
                , border3 (px 0) solid (rgb 255 255 255)
                , boxSizing borderBox
                , boxShadow5 (px 0) (px 0) (px 0) (px 2) (rgb 92 155 200)
                , zIndex (int 1000)
                ]
            ]
        , disabled
            [ color (rgb 128 128 128)
            , borderColor (rgb 128 128 128)
            ]
        ]


activePoseStyle : Style
activePoseStyle =
    Css.batch
        [ color (rgb 250 250 250)
        , backgroundColor (rgb 51 52 53)
        ]


conditionalCss : Style -> Bool -> Attribute msg
conditionalCss style condition =
    if condition then
        css [ style ]

    else
        Attr.classList []


view : Model -> Html Msg
view model =
    div
        [ css
            [ Css.batch
                [ displayFlex
                , justifyContent center
                , margin (px 20)
                ]
            ]
        ]
        [ button
            [ Attr.id "startStanding"
            , onClick (ClickedPose Stand)
            , css [ buttonStyle ]
            , conditionalCss activePoseStyle (model.currentPose == Stand)
            ]
            [ text "Stand" ]
        , div
            [ Attr.id "timer"
            , css
                [ Css.batch
                    [ displayFlex
                    , flexDirection column
                    , padding (px 5)
                    ]
                ]
            ]
            [ currentTimeText model
            , div
                [ css
                    [ Css.batch
                        [ displayFlex
                        , flexWrap wrap
                        , justifyContent center
                        , flexDirection column
                        ]
                    ]
                ]
                [ button [ Attr.id "toggleTimerMode", onClick ClickedTimerModeToggle, css [ buttonStyle, flex (num 1) ] ] [ text "Timer mode" ]
                , div [ css [ displayFlex, flexDirection row, flex (num 1) ] ]
                    [ button [ Attr.id "stopTimer", onClick (ClickedTimerState Stopped), Attr.disabled (model.timerState == Stopped), css [ buttonStyle, flex (num 1) ] ] [ text "Stop" ]
                    , button [ Attr.id "continueTimer", onClick (ClickedTimerState Running), Attr.disabled (model.timeValue == model.timeElapsed || model.timerState == Running), css [ buttonStyle, flex (num 1) ] ] [ text "Continue" ]
                    ]
                ]
            ]
        , button
            [ Attr.id "startSitting"
            , onClick (ClickedPose Sit)
            , css [ buttonStyle ]
            , conditionalCss activePoseStyle (model.currentPose == Sit)
            ]
            [ text "Sit" ]
        ]


type TimerState
    = Stopped
    | Running


type TimerMode
    = Elapsed
    | Remaining


type alias Model =
    { timeValue : Int
    , currentPose : Pose
    , timeElapsed : Int
    , timerState : TimerState
    , timerMode : TimerMode
    }


initialModel : Model
initialModel =
    { timeValue = 0
    , currentPose = Neutral
    , timeElapsed = 0
    , timerState = Stopped
    , timerMode = Elapsed
    }


type alias NotificationData =
    { title : String
    , body : String
    }


port showNotification : NotificationData -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPose pose ->
            case pose of
                Stand ->
                    ( { model | timeValue = 15 * 60, currentPose = Stand, timerState = Running, timeElapsed = 0 }, Cmd.none )

                Neutral ->
                    ( { model | timeValue = 0, currentPose = Neutral, timerState = Stopped, timeElapsed = 0 }, Cmd.none )

                Sit ->
                    ( { model | timeValue = 45 * 60, currentPose = Sit, timerState = Running, timeElapsed = 0 }, Cmd.none )

        Tick _ ->
            if model.timeElapsed >= model.timeValue && model.timeValue > 0 then
                ( { model | timerState = Stopped }, showNotification { title = "Time is up!", body = "Change your pose NOW!" } )

            else
                ( { model | timeElapsed = model.timeElapsed + 1 }, Cmd.none )

        ClickedTimerModeToggle ->
            let
                newTimerMode =
                    case model.timerMode of
                        Elapsed ->
                            Remaining

                        Remaining ->
                            Elapsed
            in
            ( { model | timerMode = newTimerMode }, Cmd.none )

        ClickedTimerState timerState ->
            ( { model | timerState = timerState }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timerState == Stopped then
        Sub.none

    else
        Time.every 1000 Tick
