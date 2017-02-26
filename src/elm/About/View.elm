module About.View exposing (..)

import About.Types exposing (..)
import Html exposing (Html, div, text, img)
import Html.Attributes exposing (class, src)


view : Model -> Html Msg
view model =
    div [ class "About" ]
        [ text "About page"
        ]
