port module Main exposing (main)

import Browser
import Home exposing (home)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Msg exposing (Msg(..))
import VitePluginHelper


main : Program () Int Msg
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Int -> ( Int, Cmd msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )


view : Int -> Html Msg
view model =
    div []
        [ img [ src <| VitePluginHelper.asset "/src/assets/logo.png", style "width" "300px" ] []
        , home model
        ]


subscriptions : Int -> Sub msg
subscriptions _ =
    Sub.none


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
