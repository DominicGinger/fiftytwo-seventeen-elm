module Main exposing (..)

import Html exposing (Html, program, div, text, button, h1, h2)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Time exposing (Time)


main : Program Never Model Msg
main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { value : Int
    , isTiming : Bool
    , isWorkTime : Bool
    }


type Msg
    = Increment Int
    | PausePlay
    | Reset


init : ( Model, Cmd Msg )
init =
    ( (Model 0 False True), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment amount ->
            let
                currentTime =
                    model.value + amount
            in
                if
                    (model.isWorkTime && currentTime >= 52 * 60)
                        || (not model.isWorkTime && currentTime >= 17 * 60)
                then
                    ( { model | value = 0, isWorkTime = not model.isWorkTime }, Cmd.none )
                else
                    ( { model | value = currentTime }, Cmd.none )

        PausePlay ->
            ( { model | isTiming = not model.isTiming }, Cmd.none )

        Reset ->
            ( { model | value = 0, isTiming = False }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        seconds =
            model.value % 60

        minutes =
            (floor ((toFloat model.value) / 60)) % 60

        hours =
            (floor ((toFloat model.value) / 3600)) % 24

        present num =
            String.padLeft 2 '0' (toString num)

        buttonStyling =
            [ ( "width", "85px" )
            , ( "padding", "10px" )
            , ( "font-size", "20px" )
            , ( "border", "2px solid black" )
            , ( "margin", "5px" )
            , ( "cursor", "pointer" )
            , ( "color", "white" )
            ]

        titleStyling =
            if model.value % 2 == 0 then
                [ ( "height", "40px" ), ( "color", "red" ) ]
            else
                [ ( "height", "40px" ), ( "color", "green" ) ]

        titleMessage =
            if model.isTiming then
                if model.isWorkTime then
                    h1 [ style [ ( "height", "40px" ), ( "color", "#545454" ) ] ] [ text "WORK" ]
                else
                    h1 [ style [ ( "height", "40px" ), ( "color", "#00a4a6" ) ] ] [ text "PLAY" ]
            else
                h1 [ style [ ( "height", "40px" ) ] ] []

        playPauseButton =
            if model.isTiming then
                button [ style (( "background-color", "#f64747" ) :: buttonStyling), onClick PausePlay ] [ text "Pause" ]
            else
                button [ style (( "background-color", "#00aa55" ) :: buttonStyling), onClick PausePlay ] [ text "Play" ]
    in
        div [ style [ ( "text-align", "center" ), ( "font-family", "helvetica, ariel, sans-serif" ) ] ]
            [ div []
                [ div [] [ titleMessage ]
                , h2 [ style [ ( "font-size", "72px" ) ] ] [ text (present hours ++ ":" ++ present minutes ++ ":" ++ present seconds) ]
                , playPauseButton
                , button [ style (( "background-color", "#939393" ) :: buttonStyling), onClick Reset ] [ text "Reset" ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isTiming then
        Time.every Time.second (\_ -> Increment 300)
    else
        Sub.none
