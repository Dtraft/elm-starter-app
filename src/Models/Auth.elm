module Models.Auth exposing (..)

import RemoteData exposing (WebData, RemoteData(..), isSuccess)
import Models.User exposing (User)


type alias Auth =
    { token : Maybe String
    , user : WebData User
    }


getAuthToken : Auth -> String
getAuthToken auth =
    auth.token
        |> Maybe.withDefault ""
