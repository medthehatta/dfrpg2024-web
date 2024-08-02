module Home exposing (cmdReplyDecoder, home, modelDecoderFromGameResult)

import Color
import FontAwesome as Icon
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Html exposing (Attribute, Html, a, button, code, div, h1, i, img, input, li, p, span, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (keyCode, on, onClick, onMouseOut, onMouseOver)
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


home : List Entity -> TurnOrder -> Maybe EntityName -> Html Msg
home entities order fpHovered =
    div [ Attr.class "home-container" ]
        [ turnOrderV entities order
        , div [ Attr.class "entity-list" ]
            (entities
                |> List.sortBy
                    (\e ->
                        ( if e.isPc then
                            0

                          else
                            1
                        , e.name
                        )
                    )
                |> List.map (\e -> entityV e (fpHovered == Just e.name))
            )
        ]


turnOrderV : List Entity -> TurnOrder -> Html Msg
turnOrderV entities order =
    let
        portrait name =
            List.filter (\x -> x.name == name) entities
                |> List.map (\x -> x.portrait)
                |> List.head
                |> Maybe.withDefault ""

        isCurrent name =
            List.indexedMap
                (\i ->
                    \x ->
                        i == Maybe.withDefault -1 order.current && x == name
                )
                order.orderedNames
                |> List.any (\s -> s == True)

        entityBox name =
            let
                currentMarker =
                    if isCurrent name then
                        Icon.view <| Icon.styled [ Icon.fa3x ] Icon.sortDown

                    else
                        div [] []
            in
            div [ Attr.class "order-box" ]
                [ div [ Attr.class "order-current" ] [ currentMarker ]
                , div [ Attr.class "order-portrait" ]
                    [ img [ Attr.src (portrait name) ] []
                    ]
                , div [ Attr.class "order-name" ] [ text name ]
                ]

        deferredBox name =
            div [ Attr.class "deferred-box" ]
                [ div [ Attr.class "deferred-portrait" ] [ img [ Attr.src (portrait name) ] [] ]
                , div [ Attr.class "order-name" ] [ text name ]
                ]

        deferredToggle =
            if order.deferredNames == [] then
                Attr.style "display" "none"

            else
                Attr.class "not-disabled"

        orderToggle =
            if order.orderedNames == [] && order.deferredNames == [] then
                Attr.style "display" "none"

            else
                Attr.class "not-disabled"
    in
    div [ Attr.class "order-track", orderToggle ]
        [ div [ Attr.class "order-main" ] (List.map entityBox order.orderedNames)
        , div [ Attr.class "deferred-wrapper", deferredToggle ]
            [ div [ Attr.class "deferred-header" ] [ text "Deferred" ]
            , div [ Attr.class "order-deferred" ]
                (List.map deferredBox order.deferredNames)
            ]
        ]


entityV : Entity -> Bool -> Html Msg
entityV entity fpHovered =
    div [ Attr.class "entity" ]
        [ div [ Attr.class "entity-header" ]
            [ div [ Attr.class "entity-portrait" ]
                [ img [ Attr.src entity.portrait ] []
                ]
            , div [ Attr.class "entity-top-corner" ]
                [ div [ Attr.class "entity-name" ] [ text entity.name ]
                , fateContainerV entity.name entity.fate fpHovered
                ]
            , if entity.stresses == [] then
                div [] []

              else
                stressContainerV entity
            ]
        , div [ Attr.class "entity-aspects" ]
            (List.map (aspectV entity.name) entity.aspects ++ [ aspectInput entity.name ])
        ]


fateContainerV : EntityName -> { refresh : Int, available : Int } -> Bool -> Html Msg
fateContainerV entityName { refresh, available } expanded =
    let
        refreshItems =
            if refresh /= 0 then
                [ span [ Attr.class "entity-fp-slash" ] [ text "/" ]
                , span [ Attr.class "entity-fp-refresh" ] [ text <| String.fromInt refresh ]
                ]

            else
                []

        withCarets html =
            div [ Attr.class "entity-carats" ]
                [ span [ onClick (IncrementFP entityName) ] [ Icon.view Icon.caretUp ]
                , html
                , span [ onClick (DecrementFP entityName) ] [ Icon.view Icon.caretDown ]
                ]

        maybeWithCarets html =
            if expanded then
                withCarets html

            else
                html

        hoverableFP html =
            div [ onMouseOver (HoverFP entityName), onMouseOut NoHoverFP ]
                [ html ]
    in
    if available == 0 && refresh == 0 && not expanded then
        hoverableFP <|
            maybeWithCarets <|
                div [ Attr.class "entity-fp" ] []

    else
        hoverableFP <|
            maybeWithCarets <|
                div [ Attr.class "entity-fp" ]
                    [ span [ Attr.class "entity-fp-avail" ] [ text (String.fromInt available) ]
                    , span [ Attr.class "entity-fp-title" ] [ text "FP" ]
                    ]


stressContainerV : Entity -> Html Msg
stressContainerV entity =
    div [ Attr.class "entity-stress-container" ]
        [ div [ Attr.class "entity-stress" ]
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
        , input [ Attr.class "aspect-text", Attr.placeholder "Add Aspect..." ] []
        , a [ Attr.class "aspect-button", Attr.href "#" ] [ Icon.view Icon.check ]
        ]



-- JSON Decoders


aspectDecoder =
    let
        aspectKindDecoder =
            D.string
                |> D.andThen
                    (\mstr ->
                        case mstr of
                            "mild" ->
                                D.succeed <| Consequence Mild

                            "moderate" ->
                                D.succeed <| Consequence Moderate

                            "severe" ->
                                D.succeed <| Consequence Severe

                            "fragile" ->
                                D.succeed Fragile

                            "sticky" ->
                                D.succeed Sticky

                            str ->
                                D.fail ("Unknown aspect kind: " ++ str)
                    )
    in
    D.map3
        Aspect
        (D.field "name" D.string)
        (D.maybe (D.field "kind" aspectKindDecoder)
            |> D.andThen (Maybe.withDefault Generic >> D.succeed)
        )
        (D.field "tags" D.int)


stressesDecoder =
    let
        stressContentDecoder =
            D.map2
                (\c -> \m -> { cap = m, used = c })
                (D.field "checked" (D.list D.int))
                (D.field "max" D.int)
    in
    D.keyValuePairs stressContentDecoder
        |> D.map
            (List.map (\( stress, v ) -> { name = stress, cap = v.cap, used = v.used }))


entityDecoder =
    let
        elderSignPortrait =
            "https://imgur.com/WPk0XRj.png"
    in
    D.map6
        Entity
        (D.field "name" D.string)
        (D.maybe (D.field "portrait" D.string)
            |> D.andThen (Maybe.withDefault elderSignPortrait >> D.succeed)
        )
        (D.field "stress" stressesDecoder)
        (D.map2
            (\x -> \y -> { refresh = x, available = y })
            (D.field "refresh" D.int)
            (D.field "fate" D.int)
        )
        (D.field "aspects" <| D.list aspectDecoder)
        (D.maybe (D.field "is_pc" <| D.bool)
            |> D.andThen (Maybe.withDefault False >> D.succeed)
        )


entityDecoderFromGameResult =
    D.at [ "result", "entities" ] (D.keyValuePairs entityDecoder)
        |> D.map (List.map Tuple.second)


orderDecoderFromGameResult =
    D.at [ "result", "order" ] <|
        D.map3
            TurnOrder
            (D.field "order" (D.list D.string))
            (D.field "deferred" (D.list D.string))
            (D.field "current" (D.nullable D.int))


modelDecoderFromGameResult =
    D.map2 GameModel entityDecoderFromGameResult orderDecoderFromGameResult


cmdReplyDecoder =
    D.at [ "result", "ok" ] D.bool
