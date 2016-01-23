import Check exposing (..)
import List exposing (..)
import Check exposing (Evidence (..), UnitEvidence, SuccessOptions, FailureOptions)
import Check.Investigator exposing (..)
import Html exposing (Html, Attribute, div, text, ul, ol, li)
import Html.Attributes exposing (style)
import Check.Runner.Browser exposing (display)

claim_multiplication_division_inverse =
  claim
    "Multiplication and division are inverse operations"
  `that`
    (\(x, y) -> x * y / y)
  `is`
    (\(x, y) -> x)
  `for`
    tuple (float, float)


suite_reverse =
  suite "Multi Div are inverses"
    [ claim_multiplication_division_inverse
    ]

result = quickCheck suite_reverse

main = display result

