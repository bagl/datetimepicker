module Main exposing (main)

import Browser
import DateTime exposing (DateTime)
import DateTimePicker exposing (..)
import DateTimePicker.Config exposing (defaultDateTimePickerConfig)
import Html exposing (Html, div)


type Msg
    = DatePickerChanged DateTimePicker.State (Maybe DateTime)


type alias Model =
    { date : Maybe DateTime, state : DateTimePicker.State }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { date = Nothing, state = DateTimePicker.initialState }
    , DateTimePicker.initialCmd DatePickerChanged DateTimePicker.initialState
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg state =
    case msg of
        DatePickerChanged st dt ->
            ( { state | date = dt, state = st }, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    div []
        [ DateTimePicker.datePicker
            DatePickerChanged
            []
            model.state
            model.date
        ]
