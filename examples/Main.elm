module Main exposing (view, update, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (class)
import Perimeter exposing (Perimeter)


-- MODEL

import Task


type alias Model =
    { buttonText : String, perimeter : Perimeter }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { buttonText = "Add", perimeter = Perimeter.init }


perimeterConfig =
    { padding = 12
    , onBreach = Breached
    , onLeave = Left
    , debug = True
    }



-- UPDATE


type Msg
    = Breached
    | Left
    | PerimeterMsg Perimeter.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Breached ->
            { model | buttonText = "Loading..." } ! []

        Left ->
            { model | buttonText = "Add" } ! []

        PerimeterMsg subMsg ->
            let
                ( newPerimeterModel, maybeMsg ) =
                    Perimeter.update subMsg model.perimeter perimeterConfig
            in
                case maybeMsg of
                    Nothing ->
                        ( { model | perimeter = newPerimeterModel }, Cmd.none )

                    Just msg ->
                        ( { model | perimeter = newPerimeterModel }, Task.perform (always msg) (Task.succeed ()) )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [] [ text "some head" ]
        , Html.map PerimeterMsg (Perimeter.view perimeterConfig model.perimeter [ button [] [ text model.buttonText ] ])
        , text <| toString model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PerimeterMsg (Perimeter.subscriptions model.perimeter)



-- INIT


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
