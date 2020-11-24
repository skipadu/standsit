module StandSitTests exposing (initialTimeText, testChecker)

import Expect
import StandSit exposing (view)
import Test exposing (Test, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


testChecker : Test
testChecker =
    test "one plus two equals three" (\_ -> Expect.equal 3 (1 + 2))


initialTimeText : Test
initialTimeText =
    test "At first, the time is --:--" <|
        \() ->
            view
                |> Query.fromHtml
                |> Query.find [ Selector.id "timeText" ]
                |> Query.has [ Selector.text "--:--" ]
