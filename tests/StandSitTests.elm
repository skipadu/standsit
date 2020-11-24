module StandSitTests exposing (testChecker)

import Expect
import Test exposing (Test, test)


testChecker : Test
testChecker =
    test "one plus two equals three" (\_ -> Expect.equal 3 (1 + 2))
