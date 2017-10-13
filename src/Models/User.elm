module Models.User exposing (..)

import Json.Decode exposing (int, string, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias User =
    { firstName : String }


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "firstName" string
