
import Check exposing (..)
import Check.Runner.Browser exposing (display)

import UtilsTest


tests : Claim
tests = suite "Utils Tests"
        [ UtilsTest.tests
        ]

result : Evidence
result = quickCheck tests

main = display result
