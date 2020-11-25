module StandSit exposing (Model, Msg(..), Pose(..), initialModel, main, padLeadingZero, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import String exposing (padLeft)


type Pose
    = Stand
    | Sit


type Msg
    = ClickedPose Pose


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
        [ button [ Attr.id "startStanding", onClick (ClickedPose Stand) ] [ text "Stand" ]
        , currentTimeText model.timeValue
        , button [ Attr.id "startSitting", onClick (ClickedPose Sit) ] [ text "Sit" ]
        ]


type alias Model =
    { timeValue : Int
    }


initialModel : Model
initialModel =
    { timeValue = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPose pose ->
            case pose of
                Stand ->
                    { model | timeValue = 15 * 60 }

                Sit ->
                    { model | timeValue = 45 * 60 }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
