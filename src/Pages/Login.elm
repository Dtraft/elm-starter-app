module Pages.Login exposing (..)

import Html exposing (Html, text, div, img, a)
import Models.Auth exposing (Auth)


type alias Model =
    {}


view : Auth -> Model -> Html msg
view auth model =
    div [] [ text "Login Page" ]
