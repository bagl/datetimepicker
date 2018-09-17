module Date.Extra exposing (daysInMonth, isLeapYear)

import Date
import Time exposing (Month(..))


isLeapYear : Int -> Bool
isLeapYear year =
    ((modBy 4 year == 0) && (modBy 100 year /= 0)) || (modBy 400 year == 0)


daysInMonth : Int -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31
