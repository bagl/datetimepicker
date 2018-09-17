module DateTimePicker.Events exposing
    ( MoveData
    , onBlurWithChange
    , onMouseDownPreventDefault
    , onMouseMoveWithPosition
    , onMouseUpPreventDefault
    , onPointerMoveWithPosition
    , onPointerUp
    , onTouchEndPreventDefault
    , onTouchMovePreventDefault
    , onTouchStartPreventDefault
    )

import DateTime exposing (DateTime)
import Html
import Html.Events
import Json.Decode
import Svg
import Svg.Events


onBlurWithChange : (String -> Maybe DateTime) -> (Maybe DateTime -> msg) -> Html.Attribute msg
onBlurWithChange parser tagger =
    Html.Events.on "blur"
        (Json.Decode.map (parser >> tagger) Html.Events.targetValue)


onMouseDownPreventDefault : msg -> Html.Attribute msg
onMouseDownPreventDefault msg =
    let
        eventOptions =
            { message = msg
            , preventDefault = True
            , stopPropagation = True
            }
    in
    Html.Events.custom "mousedown" (Json.Decode.succeed eventOptions)


onTouchStartPreventDefault : msg -> Html.Attribute msg
onTouchStartPreventDefault msg =
    let
        eventOptions =
            { message = msg
            , preventDefault = True
            , stopPropagation = True
            }
    in
    Html.Events.custom "touchstart" (Json.Decode.succeed eventOptions)


onMouseUpPreventDefault : msg -> Html.Attribute msg
onMouseUpPreventDefault msg =
    let
        eventOptions =
            { message = msg
            , preventDefault = True
            , stopPropagation = True
            }
    in
    Html.Events.custom "mouseup" (Json.Decode.succeed eventOptions)


onTouchEndPreventDefault : msg -> Html.Attribute msg
onTouchEndPreventDefault msg =
    let
        eventOptions =
            { message = msg
            , preventDefault = True
            , stopPropagation = True
            }
    in
    Html.Events.custom "touchend" (Json.Decode.succeed eventOptions)


onMouseMoveWithPosition : (MoveData -> Json.Decode.Decoder msg) -> Svg.Attribute msg
onMouseMoveWithPosition decoder =
    Svg.Events.on "mousemove"
        (mouseMoveDecoder |> Json.Decode.andThen decoder)


onPointerMoveWithPosition : (MoveData -> Json.Decode.Decoder msg) -> Svg.Attribute msg
onPointerMoveWithPosition decoder =
    Html.Events.on "pointermove"
        (mouseMoveDecoder |> Json.Decode.andThen decoder)


onPointerUp : msg -> Html.Attribute msg
onPointerUp msg =
    Html.Events.on "pointerup" (Json.Decode.succeed msg)


onTouchMovePreventDefault : msg -> Svg.Attribute msg
onTouchMovePreventDefault msg =
    let
        eventOptions =
            { message = msg
            , preventDefault = True
            , stopPropagation = True
            }
    in
    Html.Events.custom "touchstart"
        (Json.Decode.succeed eventOptions)


type alias MoveData =
    { offsetX : Int, offsetY : Int }


mouseMoveDecoder : Json.Decode.Decoder MoveData
mouseMoveDecoder =
    Json.Decode.map2 MoveData
        (Json.Decode.field "offsetX" Json.Decode.float |> Json.Decode.map round)
        (Json.Decode.field "offsetY" Json.Decode.float |> Json.Decode.map round)


touches : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
touches decoder =
    let
        loop idx xs =
            Json.Decode.maybe (Json.Decode.field (String.fromInt idx) decoder)
                |> Json.Decode.andThen
                    (Maybe.map (\x -> loop (idx + 1) (x :: xs))
                        >> Maybe.withDefault (Json.Decode.succeed xs)
                    )
    in
    Json.Decode.at [ "touches", "0" ] <| loop 0 []
