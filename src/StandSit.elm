module StandSit exposing (Model, Msg(..), Pose(..), initialModel, main, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type Pose
    = Stand
    | Sit


type Msg
    = ClickedPose Pose


view : Model -> Html Msg
view model =
    div []
        [ button [ Attr.id "startStanding", onClick (ClickedPose Stand) ] [ text "Stand" ]
        , span [ Attr.id "timeText" ] [ text model.timeString ]
        , button [ Attr.id "startSitting", onClick (ClickedPose Sit) ] [ text "Sit" ]
        ]


type alias Model =
    { timeString : String }


initialModel : Model
initialModel =
    { timeString = "--:--" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPose pose ->
            case pose of
                Stand ->
                    { model | timeString = "15:00" }

                Sit ->
                    { model | timeString = "45:00" }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
