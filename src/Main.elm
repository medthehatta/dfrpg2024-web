port module Main exposing (main)

import Browser
import Dict
import FontAwesome as Icon
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Home exposing (cmdReplyDecoder, home, modelDecoderFromGameResult)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Http
import Json.Encode as Encode
import Model exposing (..)
import Msg exposing (Msg(..))
import VitePluginHelper


refreshGameData : Cmd Msg
refreshGameData =
    Http.get
        { url = "http://mancer.in:6501/game"
        , expect = Http.expectJson GotGameData modelDecoderFromGameResult
        }


serverCmd name props =
    let
        cmdBody =
            Encode.object <|
                [ ( "command", Encode.string name ) ]
                    ++ props
    in
    Http.post
        { url = "http://mancer.in:6501/commands"
        , body = Http.jsonBody cmdBody
        , expect = Http.expectJson GotCmdReply cmdReplyDecoder
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
                ( {game = { entities = [], order = initialOrder}, ui = { fpHovered = Nothing } }
                , refreshGameData
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        issueCmd name props =
            ( model, serverCmd name props )
    in
    case Debug.log "msg=" msg of
        -- Server requests from UI
        UseStressBox entityName stressName box ->
            issueCmd
                "add_stress"
                [ ( "stress", Encode.string stressName )
                , ( "box", Encode.int box )
                , ( "entity", Encode.string entityName )
                ]

        FreeStressBox entityName stressName box ->
            issueCmd
                "clear_stress_box"
                [ ( "stress", Encode.string stressName )
                , ( "box", Encode.int box )
                , ( "entity", Encode.string entityName )
                ]

        IncrementFP entityName ->
            issueCmd
                "increment_fp"
                [ ( "entity", Encode.string entityName ) ]

        DecrementFP entityName ->
            issueCmd
                "decrement_fp"
                [ ( "entity", Encode.string entityName ) ]

        AddAspect entityName aspectName ->
            issueCmd
                "add_aspect"
                [ ( "name", Encode.string aspectName )
                , ( "entity", Encode.string entityName )
                ]

        RemoveAspect entityName aspectName ->
            issueCmd
                "remove_aspect"
                [ ( "name", Encode.string aspectName )
                , ( "entity", Encode.string entityName )
                ]

        -- HTTP Responses
        GotGameData (Err error) ->
            let
                _ =
                    Debug.log "error: " error
            in
            ( model, Cmd.none )

        GotGameData (Ok game_) ->
            ( { model | game = game_}, Cmd.none )

        GotCmdReply (Ok True) ->
            ( model, refreshGameData )

        GotCmdReply (Ok False) ->
            let
                _ =
                    Debug.log "error found" ""
            in
            ( model, Cmd.none )

        -- Internal UI Messages

        HoverFP entityName ->
            let
                updatedUi = { fpHovered = Just entityName }
            in
            ( { model | ui = updatedUi }, Cmd.none )

        NoHoverFP ->
            let
                updatedUi = { fpHovered = Nothing }
            in
            ( { model | ui = updatedUi }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Icon.css
        , home model.game.entities model.game.order model.ui.fpHovered
        ]


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
