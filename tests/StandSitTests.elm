module StandSitTests exposing (changingPoses, initialModelContents, initialTimeText, padLeadingZeros, poseButtonCssClasses, sittingTimeText, standingTimeText, startSittingClicked, startStandingClicked, testChecker, timerStarts)

import Expect
import StandSit exposing (Msg(..), Pose(..), initialModel, padLeadingZero, update, view)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time


testChecker : Test
testChecker =
    test "one plus two equals three" (\_ -> Expect.equal 3 (1 + 2))


initialModelContents : Test
initialModelContents =
    test "Initial model is as expected" <|
        \_ ->
            Expect.equal initialModel { timeValue = 0, currentPose = Neutral, timeElapsed = 0 }


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
                |> Tuple.first
                |> .timeValue
                |> Expect.equal 900


sittingTimeText : Test
sittingTimeText =
    test "Time is changed to 45:00 (2700 seconds) in model after Sit message" <|
        \_ ->
            initialModel
                |> update (ClickedPose Sit)
                |> Tuple.first
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


changingPoses : Test
changingPoses =
    describe "Changing poses will update the model"
        [ changePoseToStand
        , changePoseToSit
        , currentPoseButtonHasCssClass
        ]


changePoseToStand : Test
changePoseToStand =
    test "Pose is changed to Stand in model" <|
        \_ ->
            initialModel
                |> update (ClickedPose Stand)
                |> Tuple.first
                |> .currentPose
                |> Expect.equal Stand


changePoseToSit : Test
changePoseToSit =
    test "Pose is changed to Sit in model" <|
        \_ ->
            initialModel
                |> update (ClickedPose Sit)
                |> Tuple.first
                |> .currentPose
                |> Expect.equal Sit


currentPoseButtonHasCssClass : Test
currentPoseButtonHasCssClass =
    describe "Pose button which is current one, has wanted CSS class"
        [ standButtonHasCssClassWhenCurrent
        , sitButtonHasCssClassWhenCurrent
        ]


standButtonHasCssClassWhenCurrent : Test
standButtonHasCssClassWhenCurrent =
    test "Stand button has the CSS class when it is current pose" <|
        \_ ->
            initialModel
                |> update (ClickedPose Stand)
                |> Tuple.first
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "startStanding" ]
                |> Query.has [ Selector.class "current-pose" ]


sitButtonHasCssClassWhenCurrent : Test
sitButtonHasCssClassWhenCurrent =
    test "Sit button has the CSS class when it is current pose" <|
        \_ ->
            initialModel
                |> update (ClickedPose Sit)
                |> Tuple.first
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.id "startSitting" ]
                |> Query.has [ Selector.class "current-pose" ]


timerStarts : Test
timerStarts =
    test "Timer will start to tick" <|
        \_ ->
            initialModel
                |> update (Tick (Time.millisToPosix 0))
                |> Tuple.first
                |> .timeElapsed
                |> Expect.equal 1
