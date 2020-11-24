module StandSit exposing (main, view)

import Html exposing (div, span, text)
import Html.Attributes as Attr


view =
    div []
        [ span [ Attr.id "timeText" ] [ text "--:--" ] ]


main =
    view
