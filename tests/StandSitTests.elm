module StandSitTests exposing (initialTimeText, padLeadingZeros, poseButtonCssClasses, sittingTimeText, standingTimeText, startSittingClicked, startStandingClicked, testChecker)

import Expect
import StandSit exposing (Model, Msg(..), Pose(..), initialModel, padLeadingZero, update, view)
import Test exposing (Test, describe, test)
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
    test "Time is changed to 15:00 (900 seconds) in model after Stand message" <|
        \_ ->
            initialModel
                |> update (ClickedPose Stand)
                |> .timeValue
                |> Expect.equal 900


sittingTimeText : Test
sittingTimeText =
    test "Time is changed to 45:00 (2700 seconds) in model after Sit message" <|
        \_ ->
            initialModel
                |> update (ClickedPose Sit)
                |> .timeValue
                |> Expect.equal 2700


startStandingClicked : Test
startStandingClicked =
    test "Standing pose event called" <|
        \() ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "startStanding" ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPose Stand)


startSittingClicked : Test
startSittingClicked =
    test "Sitting pose event called" <|
        \() ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "startSitting" ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPose Sit)


padLeadingZeros : Test
padLeadingZeros =
    test "Zero are padded correctly" <|
        \_ ->
            padLeadingZero 5
                |> Expect.equal "05"


poseButtonCssClasses : Test
poseButtonCssClasses =
    describe "Pose buttons have correct CSS classes"
        [ standButtonCssClasses
        , sitButtonCssClasses
        ]


standButtonCssClasses : Test
standButtonCssClasses =
    test "Stand button has correct CSS classes" <|
        \_ ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "startStanding" ]
                |> Query.has [ Selector.classes [ "btn", "btn-pose" ] ]


sitButtonCssClasses : Test
sitButtonCssClasses =
    test "Sit button has correct CSS classes" <|
        \_ ->
            initialModel
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "startSitting" ]
                |> Query.has [ Selector.classes [ "btn", "btn-pose" ] ]
