module DateTimePicker.Internal exposing
    ( InternalState(..)
    , Time
    , TimeIndicator(..)
    , initialState
    , initialStateWithToday
    )

import DateTime exposing (DateTime)


type InternalState
    = InternalState
        { inputFocused : Bool
        , forceClose : Bool
        , event : String
        , today : Maybe DateTime
        , titleDate : Maybe DateTime
        , date : Maybe DateTime
        , time : Time
        , hourPickerStart : Int
        , minutePickerStart : Int
        , currentAngle : Maybe Float
        , activeTimeIndicator : Maybe TimeIndicator
        }


type TimeIndicator
    = HourIndicator
    | MinuteIndicator
    | AMPMIndicator


type alias Time =
    { hour : Maybe Int, minute : Maybe Int, amPm : Maybe String }


initialState : InternalState
initialState =
    InternalState
        { inputFocused = False
        , forceClose = False
        , event = ""
        , today = Nothing
        , titleDate = Nothing
        , date = Nothing
        , time = Time Nothing Nothing Nothing
        , hourPickerStart = 1
        , minutePickerStart = 0
        , currentAngle = Nothing
        , activeTimeIndicator = Just HourIndicator
        }


initialStateWithToday : DateTime -> InternalState
initialStateWithToday today =
    InternalState
        { inputFocused = False
        , forceClose = False
        , event = ""
        , today = Just today
        , titleDate = Just <| DateTime.floor DateTime.Month today
        , date = Nothing
        , time = Time Nothing Nothing Nothing
        , hourPickerStart = 1
        , minutePickerStart = 0
        , currentAngle = Nothing
        , activeTimeIndicator = Just HourIndicator
        }
