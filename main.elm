import Html exposing (Html, program, div, text, button)
import Html.Events exposing (onClick)
import Time exposing (Time)


main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Model =
  { value : Int
  , isTiming : Bool
  , isWorkTime : Bool
  }

type Msg = Increment | PausePlay | Reset

init : (Model, Cmd Msg)
init =
  ((Model 0 False True), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      if ((model.isWorkTime && model.value == 52 * 60)
      || (not model.isWorkTime && model.value == 17 * 60)) then
        ({ model | value = 0, isWorkTime = not model.isWorkTime }, Cmd.none)
      else
        if model.isTiming then
          ({ model | value = model.value + 60 }, Cmd.none)
        else
          (model, Cmd.none)
    PausePlay ->
      ({ model | isTiming = not model.isTiming }, Cmd.none)
    Reset ->
      ({ model | value = 0, isTiming = False }, Cmd.none)
      

view : Model -> Html Msg
view model =
  let
    seconds = model.value % 60
    minutes = (floor ((toFloat model.value)/60)) % 60
    hours = (floor ((toFloat model.value)/3600)) % 24
    
    present num =
      String.padLeft 2 '0' (toString num)
  
  in
  div [] [ 
        div [] [text <| if model.isWorkTime then "WORK" else "PLAY" ]
        ,  text (present hours ++ ":" ++ present minutes ++ ":" ++ present seconds)
        , button [ onClick PausePlay ] [ text <| if model.isTiming then "Pause" else "Play" ]
        , button [onClick Reset ] [ text "Reset" ]
        ]
        
subscriptions : Model -> Sub Msg
subscriptions model =
   Time.every Time.second (\_ -> Increment)