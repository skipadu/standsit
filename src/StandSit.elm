module StandSit exposing (Model, Msg(..), Pose(..), initialModel, main, padLeadingZero, update, view)

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


view : Model -> Html Msg
view model =
    div []
        [ button [ Attr.id "startStanding", Attr.class "btn btn-pose", Attr.classList [ ( "current-pose", model.currentPose == Stand ) ], onClick (ClickedPose Stand) ] [ text "Stand" ]
        , currentTimeText model.timeValue
        , button [ Attr.id "startSitting", Attr.class "btn btn-pose", Attr.classList [ ( "current-pose", model.currentPose == Sit ) ], onClick (ClickedPose Sit) ] [ text "Sit" ]
        ]


type alias Model =
    { timeValue : Int
    , currentPose : Pose
    , timeElapsed : Int
    }


initialModel : Model
initialModel =
    { timeValue = 0
    , currentPose = Neutral
    , timeElapsed = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPose pose ->
            case pose of
                Stand ->
                    ( { model | timeValue = 15 * 60, currentPose = Stand }, Cmd.none )

                Neutral ->
                    ( { model | currentPose = Neutral }, Cmd.none )

                Sit ->
                    ( { model | timeValue = 45 * 60, currentPose = Sit }, Cmd.none )

        Tick _ ->
            ( { model | timeElapsed = model.timeElapsed + 1 }, Cmd.none )


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
    Time.every 1000 Tick
