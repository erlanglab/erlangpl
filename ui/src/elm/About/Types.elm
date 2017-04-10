module About.Types exposing (..)

import Http


type Msg
    = RequestVersion (Result Http.Error String)


type alias Model =
    ( String, String )
