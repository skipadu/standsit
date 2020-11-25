module StandSitTests exposing (initialTimeText, standingTimeText, startStandingClicked, testChecker)

import Expect
import StandSit exposing (Model, Msg(..), Pose(..), initialModel, update, view)
import Test exposing (Test, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


testChecker : Test
testChecker =
    test "one plus two equals three" (\_ -> Expect.equal 3 (1 + 2))



-- TODO: describe for gathering multiple tests related to timeText


initialTimeText : Test
initialTimeText =
    test "At first, the time is --:--" <|
        \() ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "timeText" ]
                |> Query.has [ Selector.text "--:--" ]


standingTimeText : Test
standingTimeText =
    test "Time is changed to 15:00 in model after Stand message" <|
        \_ ->
            initialModel
                |> update (ClickedPose Stand)
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "timeText" ]
                |> Query.has [ Selector.text "15:00" ]


startStandingClicked : Test
startStandingClicked =
    test "Correct pose event called" <|
        \() ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "startStanding" ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPose Stand)
