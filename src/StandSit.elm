module StandSit exposing (main)

import Html exposing (div, h1, p, text)


view model =
    div []
        [ h1 [] [ text "StandSit" ]
        , div []
            [ p [] [ text "Content here" ]
            ]
        ]


main =
    view "beginning without model"
