module Api exposing (get, post)

import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Config exposing (makeApiUrl)


get : String -> String -> Decoder a -> Http.Request a
get token url decoder =
    request "GET" token url decoder Nothing


post : String -> Value -> String -> Decoder a -> Http.Request a
post token payload url decoder =
    request "POST" token url decoder (Just payload)


request : String -> String -> String -> Decoder a -> Maybe Value -> Http.Request a
request method token url decoder maybeBody =
    let
        headers =
            [ Http.header "Authorization" token ]

        body =
            maybeBody
                |> Maybe.map Http.jsonBody
                |> Maybe.withDefault Http.emptyBody
    in
        Http.request
            { method = method
            , headers = headers
            , url = makeApiUrl url
            , body = body
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
