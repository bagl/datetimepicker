module DateTimePicker.DateUtils exposing
    ( Day
    , MonthType(..)
    , dayToInt
    , fromMillitaryAmPm
    , fromMillitaryHour
    , generateCalendar
    , padding
    , setTime
    , toDate
    , toMillitary
    , toTime
    )

import Date
import Date.Extra
import DateTime exposing (DateTime)
import String
import Time exposing (Weekday(..))


dayToInt : Weekday -> Weekday -> Int
dayToInt startOfWeek day =
    let
        base =
            case day of
                Sun ->
                    0

                Mon ->
                    1

                Tue ->
                    2

                Wed ->
                    3

                Thu ->
                    4

                Fri ->
                    5

                Sat ->
                    6
    in
    case startOfWeek of
        Sun ->
            base

        Mon ->
            modBy 7 (base - 1)

        Tue ->
            modBy 7 (base - 2)

        Wed ->
            modBy 7 (base - 3)

        Thu ->
            modBy 7 (base - 4)

        Fri ->
            modBy 7 (base - 5)

        Sat ->
            modBy 7 (base - 6)


calculateNumberOfDaysForPreviousMonth : Int -> Int
calculateNumberOfDaysForPreviousMonth firstDayInInt =
    if firstDayInInt == 0 then
        7

    else
        firstDayInInt


type alias Day =
    { monthType : MonthType, day : Int }


type MonthType
    = Previous
    | Current
    | Next


generateCalendar : Time.Weekday -> Time.Month -> Int -> List Day
generateCalendar firstDayOfWeek month year =
    let
        firstDateOfMonth =
            DateTime.fromCalendarDateTime year month 1 0 0 0

        firstDayOfMonth =
            firstDateOfMonth
                |> DateTime.weekday
                |> dayToInt firstDayOfWeek

        numberOfDaysForPreviousMonth =
            calculateNumberOfDaysForPreviousMonth firstDayOfMonth

        daysInMonth =
            Date.Extra.daysInMonth
                (DateTime.year firstDateOfMonth)
                (DateTime.month firstDateOfMonth)

        firstDateOfPreviousMonth =
            DateTime.add Date.Months -1 firstDateOfMonth

        daysInPreviousMonth =
            Date.Extra.daysInMonth
                (DateTime.year firstDateOfPreviousMonth)
                (DateTime.month firstDateOfPreviousMonth)

        previousMonth =
            List.range (daysInPreviousMonth - numberOfDaysForPreviousMonth + 1) daysInPreviousMonth
                |> List.map (Day Previous)

        currentMonth =
            List.range 1 daysInMonth
                |> List.map (Day Current)

        nextMonth =
            List.range 1 14
                |> List.map (Day Next)
    in
    List.take 42 <| previousMonth ++ currentMonth ++ nextMonth


toDateTime : Int -> Time.Month -> Day -> Int -> Int -> DateTime
toDateTime year month day hour minute =
    case day.monthType of
        Current ->
            DateTime.fromCalendarDateTime year month day.day hour minute 0

        Previous ->
            DateTime.add Date.Days -1 (DateTime.fromCalendarDateTime year month 1 hour minute 0)

        Next ->
            DateTime.add Date.Months 1 (DateTime.fromCalendarDateTime year month 1 hour minute 0)


toDate : Int -> Time.Month -> Day -> DateTime
toDate year month day =
    toDateTime year month day 0 0


toTime : Int -> Int -> String -> DateTime
toTime hour minute amPm =
    DateTime.fromCalendarDateTime
        1
        Time.Jan
        1
        (toMillitary hour amPm)
        minute
        0


setTime : DateTime -> Int -> Int -> String -> DateTime
setTime dateTime hour minute amPm =
    DateTime.setTime hour minute (toMillitary hour amPm) dateTime


padding : String -> String
padding str =
    if String.length str == 0 then
        "00"

    else if String.length str == 1 then
        "0" ++ str

    else
        str


fromMillitaryHour : Int -> Int
fromMillitaryHour hour =
    case hour of
        12 ->
            12

        0 ->
            12

        _ ->
            modBy 12 hour


fromMillitaryAmPm : Int -> String
fromMillitaryAmPm hour =
    case hour of
        12 ->
            "PM"

        0 ->
            "AM"

        _ ->
            if hour >= 12 then
                "PM"

            else
                "AM"


toMillitary : Int -> String -> Int
toMillitary hour amPm =
    case ( hour, amPm ) of
        ( 12, "AM" ) ->
            0

        ( 12, "PM" ) ->
            12

        ( _, "PM" ) ->
            hour + 12

        ( _, _ ) ->
            hour
