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


type PageMsg
    = LoginMsg Login.Msg


type Route
    = Login
    | App
    | NotFound
    | Authenticating


initPageForRoute : Auth -> Route -> ( Page, Cmd PageMsg )
initPageForRoute auth route =
    case route of
        Login ->
            Login.init auth
                |> Tuple.mapFirst LoginPage
                |> Tuple.mapSecond (Cmd.map LoginMsg)

        App ->
            ( AppPage, Cmd.none )

        NotFound ->
            ( NotFoundPage, Cmd.none )

        Authenticating ->
            ( AuthenticatingPage, Cmd.none )


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
    { auth : Auth
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
            { auth = Auth flags.token nextUser
            , page = AuthenticatingPage
            }
    in
        ( nextModel, authCmd )



---- UPDATE ----


type Msg
    = UrlChange Location
    | AuthResponse Route (WebData User)
    | SetAuth Auth
    | RootPageMsg PageMsg


authenticate : String -> Route -> Cmd Msg
authenticate token redirect =
    Api.get token "/users/me" decodeUser
        |> RemoteData.sendRequest
        |> Cmd.map (AuthResponse redirect)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange nextLocation ->
            let
                nextRoute =
                    parseLocation nextLocation

                ( nextPage, nextPageMsg ) =
                    initPageForRoute model.auth nextRoute
            in
                ( { model | page = nextPage }, Cmd.map RootPageMsg nextPageMsg )

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

        SetAuth nextAuth ->
            ( { model | auth = nextAuth }, setToken (getAuthToken nextAuth) )

        RootPageMsg pageMsg ->
            let
                ( nextPage, nextPageCmd, outMsgs, outCmd ) =
                    updatePage pageMsg model

                nextCmds =
                    Cmd.batch [ Cmd.map RootPageMsg nextPageCmd, outCmd ]
            in
                List.foldl recursiveUpdate ( { model | page = nextPage }, nextCmds ) outMsgs


recursiveUpdate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
recursiveUpdate msg ( model, cmd ) =
    let
        ( nextModel, nextCmd ) =
            update msg model
    in
        ( nextModel, Cmd.batch [ cmd, nextCmd ] )


updatePage : PageMsg -> Model -> ( Page, Cmd PageMsg, List Msg, Cmd Msg )
updatePage pageMsg model =
    case ( pageMsg, model.page ) of
        ( LoginMsg subMsg, LoginPage subModel ) ->
            let
                props =
                    { setAuthTagger = SetAuth }
            in
                Login.update props subMsg subModel
                    |> mapPageUpdate LoginPage LoginMsg

        ( _, _ ) ->
            ( model.page, Cmd.none, [], Cmd.none )


mapPageUpdate : (a -> Page) -> (b -> PageMsg) -> ( a, Cmd b, List Msg, Cmd Msg ) -> ( Page, Cmd PageMsg, List Msg, Cmd Msg )
mapPageUpdate pageModelTagger pageMsgTagger ( page, pageMsgCmd, msgs, cmds ) =
    ( pageModelTagger page, Cmd.map pageMsgTagger pageMsgCmd, msgs, cmds )



---- VIEW ----


view : Model -> Html Msg
view model =
    viewPage model
        |> Html.map RootPageMsg


viewPage : Model -> Html PageMsg
viewPage model =
    case model.page of
        LoginPage pageModel ->
            Login.view model.auth pageModel
                |> Html.map LoginMsg

        _ ->
            div []
                [ div [] [ text (pageToString model.page) ]
                , a [ href "#/login" ] [ text "to login" ]
                ]


pageToString : Page -> String
pageToString page =
    case page of
        AppPage ->
            "app"

        LoginPage _ ->
            "login"

        NotFoundPage ->
            "not found"

        AuthenticatingPage ->
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
