module About exposing (..)

import About.Types exposing (..)
import About.View exposing (view)
import Html exposing (Html, text, button, div)
import Http
import Json.Decode as Decode


init : String -> ( Model, Cmd Msg )
init logo =
    ( ( logo, "0.4.0" ), getTodos )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( logo, vsn ) =
    case msg of
        RequestVersion result ->
            let
                version =
                    Result.withDefault vsn result
            in
                ( ( logo, version ), Cmd.none )


getTodos : Cmd Msg
getTodos =
    Http.send RequestVersion <|
        Http.get "http://localhost:8000/epl_version_EPL" <|
            Decode.field "version" Decode.string


main : Program String Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
