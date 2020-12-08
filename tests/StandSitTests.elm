module StandSitTests exposing (changingPoses, initialModelContents, initialTimeText, padLeadingZeros, sittingTimeText, standingTimeText, startSittingClicked, startStandingClicked, testChecker, timerModeChanged, timerStarts, timerStateChangedRunning)

import Expect
import Html.Styled exposing (div, toUnstyled)
import StandSit exposing (Msg(..), Pose(..), TimerMode(..), TimerState(..), initialModel, padLeadingZero, update, view)
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
            Expect.equal initialModel
                { timeValue = 0
                , currentPose = Neutral
                , timeElapsed = 0
                , timerState = Stopped
                , timerMode = Elapsed
                }


initialTimeText : Test
initialTimeText =
    test "At first, the time is --:--" <|
        \() ->
            initialModel
                |> (div [] << .body << view)
                |> toUnstyled
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
                |> (div [] << .body << view)
                >> toUnstyled
                |> Query.fromHtml
                |> Query.find [ Selector.id "startStanding" ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPose Stand)


startSittingClicked : Test
startSittingClicked =
    test "Sitting pose event called" <|
        \() ->
            initialModel
                |> (div [] << .body << view)
                >> toUnstyled
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


changingPoses : Test
changingPoses =
    describe "Changing poses will update the model"
        [ changePoseToStand
        , changePoseToSit
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


timerStarts : Test
timerStarts =
    test "Timer will start to tick" <|
        \_ ->
            initialModel
                |> update (Tick (Time.millisToPosix 0))
                |> Tuple.first
                |> .timeElapsed
                |> Expect.equal 1


timerModeChanged : Test
timerModeChanged =
    test "Timer mode is changed to Remaining from default Elapsed" <|
        \_ ->
            initialModel
                |> update ClickedTimerModeToggle
                |> Tuple.first
                |> .timerMode
                |> Expect.equal Remaining


timerStateChangedRunning : Test
timerStateChangedRunning =
    test "Timer state is changed to Running from default Stopped" <|
        \_ ->
            initialModel
                |> update (ClickedTimerState Running)
                |> Tuple.first
                |> .timerState
                |> Expect.equal Running
