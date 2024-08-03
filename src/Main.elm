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

        initialModel =
            { fpHovered = Nothing
            , aspectInProgress = Nothing
            , game = { entities = [], order = initialOrder }
            }
    in
    Browser.element
        { init = \_ -> ( initialModel, refreshGameData )
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
            ( { model | game = game_ }, Cmd.none )

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
                updatedUi =
                    if model.fpHovered == Just entityName then
                        { model | fpHovered = Nothing }

                    else
                        { model | fpHovered = Just entityName }
            in
            ( updatedUi, Cmd.none )

        NoHoverFP ->
            let
                updatedUi =
                    { model | fpHovered = Nothing }
            in
            ( updatedUi, Cmd.none )

        EditAspectText entityName text ->
            let
                edit =
                    { entity = entityName, name = text, kind = Generic }
            in
            case model.aspectInProgress of
                Nothing ->
                    ( { model | aspectInProgress = Just edit }, Cmd.none )

                Just { entity, name, kind } ->
                    let
                        newAIP =
                            { entity = entityName, name = text, kind = kind }
                    in
                    ( { model | aspectInProgress = Just newAIP }, Cmd.none )

        EditAspectKind entityName newKind ->
            let
                edit =
                    { entity = entityName, name = "", kind = newKind }
            in
            case model.aspectInProgress of
                Nothing ->
                    ( { model | aspectInProgress = Just edit }, Cmd.none )

                Just { entity, name, kind } ->
                    let
                        newAIP =
                            { entity = entityName, name = name, kind = newKind }
                    in
                    ( { model | aspectInProgress = Just newAIP }, Cmd.none )

        CommitAspectInProgress ->
            case model.aspectInProgress of
                Nothing ->
                    ( model, Cmd.none )

                Just { entity, name, kind } ->
                    let
                        kindStr =
                            case kind of
                                Generic ->
                                    ""

                                Fragile ->
                                    "fragile"

                                Sticky ->
                                    "sticky"

                                Consequence Mild ->
                                    "mild"

                                Consequence Moderate ->
                                    "moderate"

                                Consequence Severe ->
                                    "severe"

                        numTags =
                            1
                    in
                    issueCmd
                        "add_aspect"
                        [ ( "entity", Encode.string entity )
                        , ( "name", Encode.string name )
                        , ( "kind", Encode.string kindStr )
                        , ( "tags", Encode.int numTags )
                        ]
                        |> (\( m, c ) -> ( { m | aspectInProgress = Nothing }, c ))

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Icon.css
        , home model
        ]


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
