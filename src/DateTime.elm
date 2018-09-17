module DateTime exposing
    ( DateTime
    , Interval(..)
    , add
    , day
    , floor
    , format
    , fromCalendarDateTime
    , hmsToDaySeconds
    , hour
    , minute
    , month
    , now
    , setTime
    , weekday
    , weekdayNumber
    , year
    )

import Date exposing (Date)
import Date.Extra
import Task
import Time exposing (Month(..))


type DateTime
    = DateTime Date.Date DaySeconds


now : Task.Task Never DateTime
now =
    Task.map (\d -> DateTime d 0) Date.today


date : DateTime -> Date
date (DateTime d _) =
    d


add : Date.Unit -> Int -> DateTime -> DateTime
add unit amount (DateTime d ds) =
    DateTime (Date.add unit amount d) ds


year : DateTime -> Year
year =
    Date.year << date


month : DateTime -> Month
month =
    Date.month << date


day : DateTime -> Int
day =
    Date.day << date


hour : DateTime -> Int
hour (DateTime d ds) =
    ds // 3600


minute : DateTime -> Int
minute (DateTime d ds) =
    ds // 60


weekday : DateTime -> Time.Weekday
weekday =
    Date.weekday << date


weekdayNumber : DateTime -> Int
weekdayNumber =
    Date.weekdayNumber << date


setTime : Hours -> Minutes -> Seconds -> DateTime -> DateTime
setTime h m s (DateTime d ds) =
    DateTime d (hmsToDaySeconds h m s)


type Interval
    = Year
    | Quarter
    | Month
    | Week
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    | Day
    | Hour
    | Minute
    | Second


floor : Interval -> DateTime -> DateTime
floor int ((DateTime d ds) as dt) =
    case int of
        Hour ->
            DateTime d (ds // 3600)

        Minute ->
            DateTime d (ds // 60)

        Second ->
            dt

        Year ->
            DateTime (Date.floor Date.Year d) 0

        Quarter ->
            DateTime (Date.floor Date.Quarter d) 0

        Month ->
            DateTime (Date.floor Date.Month d) 0

        Week ->
            DateTime (Date.floor Date.Week d) 0

        Monday ->
            DateTime (Date.floor Date.Monday d) 0

        Tuesday ->
            DateTime (Date.floor Date.Tuesday d) 0

        Wednesday ->
            DateTime (Date.floor Date.Wednesday d) 0

        Thursday ->
            DateTime (Date.floor Date.Thursday d) 0

        Friday ->
            DateTime (Date.floor Date.Friday d) 0

        Saturday ->
            DateTime (Date.floor Date.Saturday d) 0

        Sunday ->
            DateTime (Date.floor Date.Sunday d) 0

        Day ->
            DateTime (Date.floor Date.Day d) 0


format : String -> DateTime -> String
format s =
    Date.format s << date


type alias DaySeconds =
    Int


type alias Year =
    Int


type alias Day =
    Int


type alias Hours =
    Int


type alias Minutes =
    Int


type alias Seconds =
    Int


fromCalendarDateTime : Year -> Month -> Day -> Hours -> Minutes -> Seconds -> DateTime
fromCalendarDateTime y m d hours minutes seconds =
    DateTime (Date.fromCalendarDate y m d) (hmsToDaySeconds hours minutes seconds)


hmsToDaySeconds : Hours -> Minutes -> Seconds -> DaySeconds
hmsToDaySeconds hours minutes seconds =
    let
        s =
            if seconds > 60 then
                59

            else
                modBy 60 seconds

        m =
            if minutes > 60 then
                59

            else
                modBy 60 minutes

        h =
            if hours > 24 then
                23

            else
                modBy 24 hours
    in
    s + m * 60 + h * 3600
