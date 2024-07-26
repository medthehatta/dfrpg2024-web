port module Main exposing (main)

import Browser
import FontAwesome as Icon
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Home exposing (home, modelDecoderFromGameResult)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Http
import Model exposing (..)
import Msg exposing (Msg(..))
import VitePluginHelper


refreshGameData : Cmd Msg
refreshGameData =
    Http.get
        { url = "http://mancer.in:6501/game"
        , expect = Http.expectJson GotGameData modelDecoderFromGameResult
        }


main : Program () Model Msg
main =
    let
        initialOrder =
            { orderedNames = [], deferredNames = [], current = Nothing }
    in
    Browser.element
        { init =
            \_ ->
                ( { entities = [], order = initialOrder }
                , refreshGameData
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg=" msg of
        GotGameData (Err error) ->
            let
                _ =
                    Debug.log "error: " error
            in
            ( model, Cmd.none )

        GotGameData (Ok model_) ->
            ( model_, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Icon.css
        , home model.entities model.order
        ]


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
