port module Main exposing (main)

import Browser
import FontAwesome as Icon
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Home exposing (entityDecoderFromGameResult, home)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Http
import Model exposing (..)
import Msg exposing (Msg(..))
import VitePluginHelper


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { entities = [] }
                , Http.get
                    { url = "http://mancer.in:6501/game"
                    , expect = Http.expectJson GotGameData entityDecoderFromGameResult
                    }
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotGameData (Err error) ->
            ( model, Cmd.none )

        GotGameData (Ok entities) ->
            ( { model | entities = entities }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Icon.css
        , home model.entities
        ]


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
