module About.View exposing (..)

import About.Types exposing (..)
import Html exposing (Html, h5, h2, h4, div, text, span, img, a, i, br)
import Html.Attributes exposing (class, src, href, target)


viewText : String -> List (Html Msg)
viewText str =
    str
        |> String.lines
        |> List.foldl (\a acc -> acc ++ [ span [] [ text a ], br [] [] ]) []


description : String
description =
    """
     It helps to observe the system and analyze its performance. If you have any idea how we can make erlangpl better,
     or you want to contribute, feel free to open issue or send PR
     """


viewVersion : String -> Html Msg
viewVersion version =
    text <|
        "Erlang Performance Lab"
            ++ if String.length version > 0 then
                " v" ++ version
               else
                version


view : Model -> Html Msg
view ( logo, version ) =
    div [ class "About text-center" ]
        [ div [ class "header text-center" ]
            [ img [ src logo ] []
            ]
        , div [ class "content" ]
            [ h2 [ class "text-center" ]
                [ viewVersion version
                ]
            , h5 [ class "text-center" ] [ text "tool for developers working with the Erlang VM (BEAM).\n" ]
            , span [] <| viewText description
            , h4 [] [ text "Links" ]
            , i [ class "fa fa-github" ] []
            , a [ href "https://github.com/erlanglab", target "_blank" ]
                [ text "GitHub" ]
            , br [] []
            , i [ class "fa fa-twitter" ] []
            , a [ href "https://twitter.com/erlanglab", target "_blank" ]
                [ text "Twitter" ]
            , br [] []
            , i [ class "fa fa-info" ] []
            , a [ href "https://erlang.pl", target "_blank" ]
                [ text "Website" ]
            , br [] []
            , i [ class "fa fa-file-text-o" ] []
            , a [ href "http://apache.org/licenses/LICENSE-2.0", target "_blank" ]
                [ text "Licence: Apache 2.0" ]
            , br [] []
            ]
        ]
