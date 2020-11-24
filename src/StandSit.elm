module StandSit exposing (Msg(..), Pose(..), main, view)

import Html exposing (button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type Pose
    = Stand


type Msg
    = ClickedPose Pose


view =
    div []
        [ button [ Attr.id "startStanding", onClick (ClickedPose Stand) ] [ text "Stand" ]
        , span [ Attr.id "timeText" ] [ text "--:--" ]
        ]


main =
    view
