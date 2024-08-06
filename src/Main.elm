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
            { edit = NotEditing
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

        RemoveAspect entityName aspectName ->
            issueCmd
                "remove_aspect"
                [ ( "entity", Encode.string entityName ), ( "name", Encode.string aspectName ) ]

        CommitAspectInProgress ->
            let
                aspectInProgress =
                    case model.edit of
                        EditingAspectString entityName aspectKind aspectStr ->
                            Just { entity = entityName, kind = aspectKind, name = aspectStr }

                        EditingAspectKind entityName aspectKind aspectStr ->
                            Just { entity = entityName, kind = aspectKind, name = aspectStr }

                        _ ->
                            Nothing
            in
            case aspectInProgress of
                Nothing ->
                    ( model, Cmd.none )

                Just { entity, name, kind } ->
                    if name == "" then
                        ( { model | edit = NotEditing }, Cmd.none )

                    else
                        let
                            kindAttr =
                                case kind of
                                    Generic ->
                                        []

                                    Fragile ->
                                        [ ( "kind", Encode.string "fragile" ) ]

                                    Sticky ->
                                        [ ( "kind", Encode.string "sticky" ) ]

                                    Consequence Mild ->
                                        [ ( "kind", Encode.string "mild" ) ]

                                    Consequence Moderate ->
                                        [ ( "kind", Encode.string "moderate" ) ]

                                    Consequence Severe ->
                                        [ ( "kind", Encode.string "severe" ) ]

                            numTags =
                                1
                        in
                        issueCmd
                            "add_aspect"
                            ([ ( "entity", Encode.string entity )
                             , ( "name", Encode.string name )
                             ]
                                ++ kindAttr
                            )
                            |> (\( m, c ) -> ( { m | edit = NotEditing }, c ))

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
                newModel =
                    case model.edit of
                        EditingFatePoints eName ->
                            if eName == entityName then
                                { model | edit = NotEditing }

                            else
                                { model | edit = EditingFatePoints entityName }

                        _ ->
                            { model | edit = EditingFatePoints entityName }
            in
            ( newModel, Cmd.none )

        NoHoverFP ->
            ( { model | edit = NotEditing }, Cmd.none )

        EditAspectText entityName text ->
            case model.edit of
                EditingAspectString _ aspectKind _ ->
                    ( { model | edit = EditingAspectString entityName aspectKind text }, Cmd.none )

                _ ->
                    ( { model | edit = EditingAspectString entityName Generic text }, Cmd.none )

        EditAspectKind entityName newKind ->
            case model.edit of
                EditingAspectString _ _ aspectStr ->
                    ( { model | edit = EditingAspectString entityName newKind aspectStr }, Cmd.none )

                _ ->
                    ( { model | edit = EditingAspectString entityName newKind "" }, Cmd.none )

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
