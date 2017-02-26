module About exposing (..)

import About.Types exposing (..)
import About.View exposing (view)
import Html exposing (Html, text, button, div)


init : String -> ( Model, Cmd Msg )
init change =
    ( change, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


main : Program String Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
