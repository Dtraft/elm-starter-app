module Pages.Login exposing (..)

import Html exposing (Html, text, div, input, button)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, onClick)
import RemoteData exposing (WebData, RemoteData(..), isSuccess, isLoading, isFailure)
import Navigation
import Api
import Models.Auth exposing (Auth, decodeAuth, getAuthToken, encodeLoginPayload)


type alias Model =
    { username : String
    , password : String
    , status : WebData Auth
    }


type alias Props msg =
    { setAuthTagger : Auth -> msg
    }


init : Auth -> ( Model, Cmd Msg )
init auth =
    ( Model "" "" NotAsked, Cmd.none )


type Msg
    = SetUsername String
    | SetPassword String
    | Login
    | LoginResponse (WebData Auth)


update : Props a -> Msg -> Model -> ( Model, Cmd Msg, List a, Cmd a )
update { setAuthTagger } msg model =
    case msg of
        SetUsername username ->
            ( { model | username = username, status = NotAsked }, Cmd.none, [], Cmd.none )

        SetPassword password ->
            ( { model | password = password, status = NotAsked }, Cmd.none, [], Cmd.none )

        Login ->
            ( { model | status = Loading }, login model, [], Cmd.none )

        LoginResponse response ->
            let
                ( outMsg, outCmd ) =
                    case response of
                        Success auth ->
                            ( [ setAuthTagger auth ], Navigation.modifyUrl "#/" )

                        _ ->
                            ( [], Cmd.none )
            in
                ( { model | status = response }, Cmd.none, outMsg, outCmd )


login : Model -> Cmd Msg
login model =
    let
        payload =
            encodeLoginPayload { username = model.username, password = model.password }
    in
        Api.post "" payload "/users/login?include=user" decodeAuth
            |> RemoteData.sendRequest
            |> Cmd.map LoginResponse


view : Auth -> Model -> Html Msg
view auth model =
    div []
        [ div []
            [ input
                [ onInput SetUsername
                , placeholder "Username"
                ]
                []
            ]
        , div []
            [ input
                [ onInput SetPassword
                , placeholder "Password"
                ]
                []
            ]
        , div []
            [ button [ onClick Login ]
                [ text
                    (if isLoading model.status then
                        "Logging In..."
                     else
                        "Login"
                    )
                ]
            , if isFailure model.status then
                div [] [ text "Error logging in." ]
              else
                text ""
            ]
        ]
