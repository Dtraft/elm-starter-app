module Models.Auth exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (int, string, float, Decoder, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import RemoteData exposing (WebData, RemoteData(..), isSuccess)
import Models.User exposing (User, decodeUser)


type alias Auth =
    { token : Maybe String
    , user : WebData User
    }


decodeAuth : Decoder Auth
decodeAuth =
    decode Auth
        |> required "id" (nullable string)
        |> required "user" (Decode.map Success decodeUser)


getAuthToken : Auth -> String
getAuthToken auth =
    auth.token
        |> Maybe.withDefault ""


encodeLoginPayload : { username : String, password : String } -> Encode.Value
encodeLoginPayload { username, password } =
    Encode.object
        [ ( "email", Encode.string username )
        , ( "password", Encode.string password )
        ]
