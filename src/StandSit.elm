module StandSit exposing (Model, Msg(..), Pose(..), TimerMode(..), TimerState(..), initialModel, main, padLeadingZero, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
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


padLeadingZero : Int -> String
padLeadingZero value =
    padLeft 2 '0' (String.fromInt value)


currentTimeText : Int -> Html Msg
currentTimeText seconds =
    let
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
            if seconds == 0 then
                "--:--"

            else
                minutes ++ ":" ++ remainingSeconds
    in
    span [ Attr.id "timeText" ] [ text timeText ]


modeBasedTime : Model -> Int
modeBasedTime model =
    case model.timerMode of
        Elapsed ->
            model.timeElapsed

        Remaining ->
            model.timeValue - model.timeElapsed


view : Model -> Html Msg
view model =
    div []
        [ button [ Attr.id "startStanding", Attr.class "btn btn-pose", Attr.classList [ ( "current-pose", model.currentPose == Stand ) ], onClick (ClickedPose Stand) ] [ text "Stand" ]
        , currentTimeText (modeBasedTime model)
        , button [ Attr.id "startSitting", Attr.class "btn btn-pose", Attr.classList [ ( "current-pose", model.currentPose == Sit ) ], onClick (ClickedPose Sit) ] [ text "Sit" ]
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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timerState == Stopped then
        Sub.none

    else
        Time.every 1000 Tick
