module MultiPanel exposing (view)

import AnalogTimePickerPanel
import Date
import DatePickerPanel
import DateTime exposing (DateTime)
import DateTimePicker.Config exposing (CssConfig, TimePickerType(..))
import DateTimePicker.DateUtils
import DateTimePicker.Internal exposing (InternalState(..))
import DateTimePicker.SharedStyles exposing (CssClasses)
import DigitalTimePickerPanel
import Html exposing (Html)


type alias State =
    InternalState


type T4 a b c d
    = T4 a b c d


view : DatePickerPanel.Config (CssConfig a msg CssClasses) msg -> ( TimePickerType, DigitalTimePickerPanel.Config (CssConfig a msg CssClasses) msg ) -> State -> Maybe DateTime -> List (Html msg)
view dateConfig ( timeType, timeConfig ) state currentDate =
    let
        safeOnChange (InternalState st) _ =
            -- we ignore the provided value
            -- (which may come from either the date or the time panel)
            -- and instead check the state
            -- to see if both a date and time have been picked
            dateConfig.onChange (InternalState st)
                (case T4 st.date st.time.hour st.time.minute st.time.amPm of
                    T4 (Just date) (Just hour) (Just minute) (Just amPm) ->
                        Just <| DateTimePicker.DateUtils.setTime date hour minute amPm

                    _ ->
                        Nothing
                )

        safeDateConfig =
            { dateConfig | onChange = safeOnChange }

        safeTimeConfig =
            { timeConfig | onChange = safeOnChange }
    in
    [ DatePickerPanel.view safeDateConfig state currentDate
    , case timeType of
        Digital ->
            DigitalTimePickerPanel.view safeTimeConfig state currentDate

        Analog ->
            AnalogTimePickerPanel.view safeTimeConfig state currentDate
    ]
