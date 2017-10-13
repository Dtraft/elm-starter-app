module Main exposing (..)

-- ELM PACKAGES

import Html exposing (Html, text, div, img, a)
import Html.Attributes exposing (href)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import RemoteData exposing (WebData, RemoteData(..), isSuccess)


-- PROJECT LIBRARIES

import LocalStorage exposing (setToken)
import Api
import Models.Auth exposing (Auth, getAuthToken)
import Models.User exposing (User, decodeUser)


-- PAGES

import Pages.Login as Login


-- ROUTING


type Page
    = LoginPage Login.Model
    | AppPage
    | NotFoundPage
    | AuthenticatingPage


type Route
    = Login
    | App
    | NotFound
    | Authenticating


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map App top
        , Url.map Login (s "login")
        ]


urlForRoute : Route -> String
urlForRoute route =
    case route of
        Login ->
            "#/login"

        App ->
            "#/"

        _ ->
            "#/misc"


parseLocation : Location -> Route
parseLocation location =
    Url.parseHash route location
        |> Maybe.withDefault NotFound



---- MODEL ----


type alias Flags =
    { token : Maybe String }


type alias Model =
    { route : Route
    , auth : Auth
    , page : Page
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        ( nextRoute, nextUser, authCmd ) =
            case flags.token of
                Just token ->
                    let
                        currentRoute =
                            parseLocation location

                        redirectRoute =
                            if currentRoute /= Login then
                                currentRoute
                            else
                                App
                    in
                        ( Authenticating, Loading, authenticate token redirectRoute )

                Nothing ->
                    ( Login, NotAsked, Cmd.none )

        nextModel =
            { route = nextRoute
            , auth = Auth flags.token nextUser
            , page = AuthenticatingPage
            }
    in
        ( nextModel, authCmd )



---- UPDATE ----


type Msg
    = UrlChange Location
    | AuthResponse Route (WebData User)


authenticate : String -> Route -> Cmd Msg
authenticate token redirect =
    Api.get token "/users/me" decodeUser
        |> RemoteData.sendRequest
        |> Cmd.map (AuthResponse redirect)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange nextLocation ->
            ( { model | route = parseLocation nextLocation }, Cmd.none )

        AuthResponse redirect response ->
            let
                nextCmd =
                    if isSuccess response then
                        Navigation.modifyUrl (urlForRoute redirect)
                    else
                        Navigation.modifyUrl "#/login"

                auth =
                    model.auth

                nextAuth =
                    { auth | user = response }
            in
                ( { model
                    | auth = nextAuth
                  }
                , nextCmd
                )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.route of
        Login ->
            Login.view model.auth {}

        _ ->
            div []
                [ div [] [ text (routeToString model.route) ]
                , a [ href "#/login" ] [ text "to login" ]
                ]


routeToString : Route -> String
routeToString route =
    case route of
        App ->
            "app"

        Login ->
            "login"

        NotFound ->
            "not found"

        Authenticating ->
            "loading"



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
