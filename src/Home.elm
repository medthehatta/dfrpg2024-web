module Home exposing (home)

import Color
import FontAwesome as Icon
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Html exposing (Attribute, Html, a, button, code, div, h1, i, input, li, p, span, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (keyCode, on, onClick)
import Json.Decode as D
import Model exposing (..)
import Msg exposing (Msg(..))


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                D.succeed msg

            else
                D.fail "not ENTER"
    in
    on "keydown" (D.andThen isEnter keyCode)


nbsp : String
nbsp =
    String.fromChar '\u{00A0}'


home : List Entity -> Html Msg
home entities =
    div [ Attr.class "entity-list" ]
        (List.map entityV entities)


entityV : Entity -> Html Msg
entityV entity =
    div [ Attr.class "entity" ]
        [ div [ Attr.class "entity-header" ]
            [ div [ Attr.class "entity-portrait" ] []
            , div [ Attr.class "entity-top-corner" ]
                [ div [ Attr.class "entity-name" ] [ text entity.name ]
                , fateContainerV entity entity.fate
                ]
            , if entity.stresses == [] then
                div [] []

              else
                stressContainerV entity entity.stresses
            ]
        , div [ Attr.class "entity-aspects" ]
            (List.map (aspectV entity.name) entity.aspects ++ [ aspectInput entity.name ])
        ]


fateContainerV : Entity -> { refresh : Int, available : Int } -> Html Msg
fateContainerV entity { refresh, available } =
    let
        refreshItems =
            if refresh /= 0 then
                [ span [ Attr.class "entity-fp-slash" ] [ text "/" ]
                , span [ Attr.class "entity-fp-refresh" ] [ text <| String.fromInt refresh ]
                ]

            else
                []
    in
    if available == 0 && refresh == 0 then
        div [ Attr.class "entity-fp" ] []

    else
        div [ Attr.class "entity-fp" ]
            ([ span [ Attr.class "entity-fp-avail" ] [ text <| String.fromInt available ]
             , span [ Attr.class "entity-fp-title" ] [ text "FP" ]
             ]
                ++ refreshItems
            )


stressContainerV : Entity -> List StressTrack -> Html Msg
stressContainerV entity stresses =
    div [ Attr.class "entity-stress-container" ]
        [ div [ Attr.class "entity-stress-header" ] [ text "Stress" ]
        , div [ Attr.class "entity-stress" ]
            (List.map (stressV entity.name) entity.stresses)
        ]


stressV : EntityName -> StressTrack -> Html Msg
stressV entityName stress =
    let
        filled slot =
            span [ Attr.class "stress-circle", Attr.class "stress-circle-filled", onClick <| FreeStressBox entityName stress.name slot ] [ text <| String.fromInt slot ]

        empty slot =
            span [ Attr.class "stress-circle", onClick <| UseStressBox entityName stress.name slot ] [ text <| String.fromInt slot ]
    in
    div [ Attr.class "stress" ]
        [ span [ Attr.class "stress-indicator" ] [ text stress.name ]
        , span [ Attr.class "stress-track" ]
            (List.map
                (\slot ->
                    if List.member slot stress.used then
                        filled slot

                    else
                        empty slot
                )
                (List.range 1 stress.cap)
            )
        ]


aspectV : EntityName -> Aspect -> Html Msg
aspectV entityName { name, kind, tags } =
    let
        headSpan =
            case kind of
                Generic ->
                    span [ Attr.class "aspect-head", Attr.class "aspect-generic" ] [ text <| nbsp ]

                Consequence sev ->
                    case sev of
                        Mild ->
                            span [ Attr.class "aspect-head", Attr.class "aspect-mild" ] [ text "mild" ]

                        Moderate ->
                            span [ Attr.class "aspect-head", Attr.class "aspect-moderate" ] [ text "mod" ]

                        Severe ->
                            span [ Attr.class "aspect-head", Attr.class "aspect-severe" ] [ text "sev" ]

                Fragile ->
                    span [ Attr.class "aspect-head", Attr.class "aspect-fragile" ] [ text "f" ]

                Sticky ->
                    span [ Attr.class "aspect-head", Attr.class "aspect-sticky" ] [ text "s" ]

        tagSpan =
            span [ Attr.class "aspect-tags", onClick <| TagAspect entityName name ] (List.repeat tags (Icon.view Icon.hashtag))
    in
    div [ Attr.class "aspect" ]
        [ headSpan
        , tagSpan
        , span [ Attr.class "aspect-text" ] [ text name ]
        , a [ Attr.class "aspect-button", Attr.href "#" ] [ Icon.view Icon.x ]
        ]


aspectInput : EntityName -> Html Msg
aspectInput entityName =
    div [ Attr.class "aspect", Attr.class "aspect-input" ]
        [ span [ Attr.class "aspect-head", Attr.class "aspect-generic" ] []
        , input [ Attr.class "aspect-text", Attr.placeholder "Add New" ] []
        , a [ Attr.class "aspect-button", Attr.href "#" ] [ Icon.view Icon.check ]
        ]
