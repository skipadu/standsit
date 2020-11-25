module StandSit exposing (Model, Msg(..), Pose(..), initialModel, main, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type Pose
    = Stand


type Msg
    = ClickedPose Pose


view : Model -> Html Msg
view model =
    div []
        [ button [ Attr.id "startStanding", onClick (ClickedPose Stand) ] [ text "Stand" ]
        , span [ Attr.id "timeText" ] [ text model.timeString ]
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
            { model | timeString = "15:00" }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
