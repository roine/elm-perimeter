module Perimeter
    exposing
        ( Perimeter
        , init
        , Config
        , view
        , Msg
        , update
        , subscriptions
        )

{-| This library helps you create an invisible perimeter around a target
element and monitors mouse breaches. This is a [Perimeter][]'s port to ELM.
[Perimeter]: https://github.com/e-sites/perimeter.js

# Type and Constructor
@docs Perimeter, Msg, Config

# State
@docs init

# View
@docs view

# Update
@docs update

# Subscriptions
@docs subscriptions
-}

import Html exposing (..)
import Html.Attributes exposing (classList, style)
import Json.Decode exposing (Decoder)
import Html.Events exposing (on)
import Mouse exposing (Position)
import Time
import Perimeter.BoundingRect as BoundingRect


{-| The configuration for perimeter.
Padding represents the padding from which the perimeter is breached.
When True Debug displays the perimeter.
-}
type alias Config msg =
    { padding : Float
    , onBreach : msg
    , onLeave : msg
    , debug : Bool
    }


type alias State =
    { breached : Bool
    , rectangle : BoundingRect.Rectangle
    , trigger : Bool
    }


{-| The Perimeter internal State
-}
type Perimeter
    = Perimeter State


{-| Initialise the Perimeter's state, default state:

    init =
        { breached = False
        , rectangle = initialRectangle
        , trigger = False
        }

    initialRectangle =
        { top = 0
        , left = 0
        , right = 0
        , bottom = 0
        , width = 0
        , height = 0
        }


-}
init : Perimeter
init =
    Perimeter
        { breached = False
        , rectangle = BoundingRect.initialRectangle
        , trigger = False
        }


{-| The perimeter's view. Example of use:

    type Msg = Breached | Left

    perimeterConfig =
        { padding = 12
        , onBreach = Breached
        , onLeave = Left
        , debug = True
        }

    Perimeter.view perimeterConfig model.perimeter [ button [] [ text "Add" ] ]
-}
view : Config msg -> Perimeter -> List (Html Msg) -> Html Msg
view ({ padding, debug } as config) (Perimeter ({ rectangle } as model)) children =
    span [ BoundingRect.getBoundingRect SetRectangle, BoundingRect.getBoundingReactStyle model.trigger ]
        (children
            ++ [ if debug then
                    viewDebugFrame padding rectangle
                 else
                    text ""
               ]
        )


viewDebugFrame : Float -> BoundingRect.Rectangle -> Html msg
viewDebugFrame padding rectangle =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", toString (rectangle.top - padding) ++ "px" )
            , ( "left", toString (rectangle.left - padding) ++ "px" )
            , ( "width", toString (rectangle.right - rectangle.left + padding * 2) ++ "px" )
            , ( "height", toString (rectangle.bottom - rectangle.top + padding * 2) ++ "px" )
            , ( "background", "rgba(0,0,255, .4)" )
            , ( "zIndex", "-1" )
            ]
        ]
        []


{-| A message type for the internal of perimeter
-}
type Msg
    = VerifyBreach Position
    | SetRectangle BoundingRect.Rectangle
    | TriggerEvent


{-| The update function returns a `Maybe Msg` that needs to be
passed to the parent's update function, like this:

    let
        ( newPerimeterModel, maybeMsg ) =
            Perimeter.update subMsg model.perimeter perimeterConfig
    in
        case maybeMsg of
            Nothing ->
                ( { model | perimeter = newPerimeterModel }, Cmd.none )

            Just msg ->
                ( { model | perimeter = newPerimeterModel }, Task.perform (always msg) (Task.succeed ()) )

The last line could be replaced by:

    update msg { model | perimeter = newPerimeterModel }
-}
update : Msg -> Perimeter -> Config msg -> ( Perimeter, Maybe msg )
update msg (Perimeter ({ rectangle } as model)) ({ padding } as config) =
    case msg of
        VerifyBreach { x, y } ->
            if
                ((toFloat x) > rectangle.left - padding)
                    && ((toFloat x) < rectangle.right + padding)
                    && ((toFloat y) > rectangle.top - padding)
                    && ((toFloat y) < rectangle.bottom + padding)
            then
                if model.breached == False then
                    ( { model | breached = True } |> Perimeter, Just config.onBreach )
                else
                    ( Perimeter model, Nothing )
            else if model.breached == False then
                ( Perimeter model, Nothing )
            else
                ( { model | breached = False } |> Perimeter, Just config.onLeave )

        SetRectangle rect ->
            ( { model | rectangle = rect } |> Perimeter, Nothing )

        TriggerEvent ->
            ( { model | trigger = True } |> Perimeter, Nothing )


{-| It is mandatory to wire this up in the parents otherwise perimeter won't work
-}
subscriptions : Perimeter -> Sub Msg
subscriptions (Perimeter model) =
    Sub.batch
        [ Mouse.moves VerifyBreach
        , if not model.trigger then
            Time.every (10 * Time.millisecond) (always TriggerEvent)
          else
            Sub.none
        ]
