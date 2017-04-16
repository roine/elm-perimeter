module Perimeter.BoundingRect exposing (Rectangle, initialRectangle, getBoundingRect, getBoundingReactStyle)

{-|
  Where the magic happens to get the boundingRect

  Force a transition to happen,
  listen to `transitionend` event,
  get all the information about the element's position,
  build a Rectangle record using those information
-}

import Html
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder)


type alias Rectangle =
    { top : Float
    , left : Float
    , right : Float
    , bottom : Float
    , width : Float
    , height : Float
    }


initialRectangle : Rectangle
initialRectangle =
    { top = 0
    , left = 0
    , right = 0
    , bottom = 0
    , width = 0
    , height = 0
    }


getBoundingRect : (Rectangle -> msg) -> Html.Attribute msg
getBoundingRect tagger =
    on "transitionend"
        (Json.Decode.map tagger
            (Json.Decode.map6 Rectangle
                (target (Json.Decode.field "offsetTop" Json.Decode.float))
                (target (Json.Decode.field "offsetLeft" Json.Decode.float))
                (Json.Decode.succeed 0)
                (Json.Decode.succeed 0)
                (target (Json.Decode.field "offsetWidth" Json.Decode.float))
                (target (Json.Decode.field "offsetHeight" Json.Decode.float))
                |> Json.Decode.andThen
                    (\rect ->
                        Json.Decode.succeed
                            { rect
                                | right = rect.left + rect.width
                                , bottom = rect.top + rect.height
                            }
                    )
            )
        )


target : Decoder a -> Decoder a
target decoder =
    Json.Decode.field "target" decoder


getBoundingReactStyle : Bool -> Html.Attribute msg
getBoundingReactStyle trigger =
    if trigger then
        style [ ( "transform", "translateZ(0)" ), ( "transition", "1ms all linear" ) ]
    else
        style [ ( "transition", "1ms all linear" ) ]
