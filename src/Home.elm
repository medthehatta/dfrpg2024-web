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
import Html.Events exposing (keyCode, on, onClick, onInput)
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


home : Model -> Html Msg
home model =
    let
        entities =
            model.game.entities
    in
    div [ Attr.class "home-container" ]
        [ turnOrderV model
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
                |> List.map (\e -> entityV model e)
            )
        ]


turnOrderV : Model -> Html Msg
turnOrderV model =
    let
        entities =
            model.game.entities

        order =
            model.game.order

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


entityV : Model -> Entity -> Html Msg
entityV model entity =
    let
        onClickEditAspect =
            onClick (EditAspect entity.name Generic "")

        addButton =
            a
                [ Attr.class "aspect-add-button", onClickEditAspect ]
                [ Icon.view Icon.plus ]

        maybeEdit =
            case model.edit of
                EditingAspectString entityName aspectKind aspectStr ->
                    if entityName == entity.name then
                        aspectV model entity { tags = 1, name = aspectStr, kind = aspectKind }

                    else
                        addButton

                EditingAspectKind entityName aspectKind aspectStr ->
                    if entityName == entity.name then
                        aspectV model entity { tags = 1, name = aspectStr, kind = aspectKind }

                    else
                        addButton

                _ ->
                    addButton
    in
    div [ Attr.class "entity" ]
        [ div [ Attr.class "entity-header" ]
            [ div [ Attr.class "entity-portrait" ]
                [ img [ Attr.src entity.portrait ] []
                ]
            , div [ Attr.class "entity-top-corner" ]
                [ div [ Attr.class "entity-name" ] [ text entity.name ]
                , fateContainerV model entity
                ]
            , if entity.stresses == [] then
                div [] []

              else
                stressContainerV model entity
            ]
        , div [ Attr.class "entity-aspects" ]
            (List.map (aspectV model entity) entity.aspects ++ [ maybeEdit ])
        ]


fateContainerV : Model -> Entity -> Html Msg
fateContainerV model entity =
    let
        entityName =
            entity.name

        refresh =
            entity.fate.refresh

        available =
            entity.fate.available

        expanded =
            case model.edit of
                EditingFatePoints eName ->
                    eName == entity.name

                _ ->
                    False

        refreshItems =
            if refresh /= 0 then
                [ span [ Attr.class "entity-fp-slash" ] [ text "/" ]
                , span [ Attr.class "entity-fp-refresh" ] [ text <| String.fromInt refresh ]
                ]

            else
                []

        maybeWithCarets html =
            if expanded then
                div []
                    [ div [ Attr.class "entity-carats", onClick (IncrementFP entityName) ] [ Icon.view Icon.caretUp ]
                    , html
                    , div [ Attr.class "entity-carats", onClick (DecrementFP entityName) ] [ Icon.view Icon.caretDown ]
                    ]

            else
                div [] [ div [ Attr.class "entity-carats" ] [], html, div [ Attr.class "entity-carats" ] [] ]

        hoverableFP html =
            div [ Attr.class "entity-hoverable-zone", onClick (HoverFP entityName) ]
                [ html ]
    in
    if available == 0 && refresh == 0 && not expanded then
        maybeWithCarets <|
            hoverableFP <|
                div [ Attr.class "entity-fp" ] []

    else
        maybeWithCarets <|
            hoverableFP <|
                div [ Attr.class "entity-fp" ]
                    [ span [ Attr.class "entity-fp-avail" ] [ text (String.fromInt available) ]
                    , span [ Attr.class "entity-fp-title" ] [ text "FP" ]
                    ]


stressContainerV : Model -> Entity -> Html Msg
stressContainerV model entity =
    div [ Attr.class "entity-stress-container" ]
        [ div [ Attr.class "entity-stress" ]
            (List.map (stressV model entity) entity.stresses)
        ]


stressV : Model -> Entity -> StressTrack -> Html Msg
stressV model entity stress =
    let
        entityName =
            entity.name

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


aspectV : Model -> Entity -> Aspect -> Html Msg
aspectV model entity aspect =
    let
        entityName =
            entity.name

        name =
            aspect.name

        kind =
            aspect.kind

        tags =
            aspect.tags

        onClickEditAspectKind k =
            case model.edit of
                EditingAspectKind eName aKind aStr ->
                    if eName == entityName && aStr == aspect.name then
                        onClick (EditAspect eName k aStr)

                    else
                        onClick (EditAspect entityName k "")

                EditingAspectString eName aKind aStr ->
                    if eName == entityName && aStr == aspect.name then
                        onClick OpenEditAspectKind

                    else
                        onClick (EditAspect entityName k "")


                _ ->
                    onClick (EditAspect entityName Generic "")

        headSpan k =
            case k of
                Generic ->
                    span [ onClickEditAspectKind k, Attr.class "aspect-head", Attr.class "aspect-generic" ] [ text <| nbsp ]

                Consequence sev ->
                    case sev of
                        Mild ->
                            span [ onClickEditAspectKind k, Attr.class "aspect-head", Attr.class "aspect-mild" ] [ text "mild" ]

                        Moderate ->
                            span [ onClickEditAspectKind k, Attr.class "aspect-head", Attr.class "aspect-moderate" ] [ text "mod" ]

                        Severe ->
                            span [ onClickEditAspectKind k, Attr.class "aspect-head", Attr.class "aspect-severe" ] [ text "sev" ]

                Fragile ->
                    span [ onClickEditAspectKind k, Attr.class "aspect-head", Attr.class "aspect-fragile" ] [ text "f" ]

                Sticky ->
                    span [ onClickEditAspectKind k, Attr.class "aspect-head", Attr.class "aspect-sticky" ] [ text "s" ]

        allKinds =
            [ Generic
            , Consequence Mild
            , Consequence Moderate
            , Consequence Severe
            , Fragile
            , Sticky
            ]

        kindEditSpan =
            case model.edit of
                EditingAspectKind eName aspectKind aspectStr ->
                    if eName == entityName then
                        ul [ Attr.class "aspect-head-listing" ] (List.map headSpan allKinds)

                    else
                        headSpan kind

                _ ->
                    headSpan kind

        tagSpan =
            span [ Attr.class "aspect-tags", onClick <| TagAspect entityName name ] (List.repeat tags (Icon.view Icon.hashtag))

        onClickX =
            onClick (RemoveAspect entityName name)

        ifAspectForMe yes no =
            case model.edit of
                EditingAspectString eName aKind aString ->
                    if eName == entityName && aString == aspect.name then
                        yes entity

                    else
                        no

                EditingAspectKind eName aKind aString ->
                    if eName == entityName && aString == aspect.name then
                        yes entity

                    else
                        no

                _ ->
                    no

        inHandler =
            onInput (EditAspect entityName Generic)

        onClickCheck =
            ifAspectForMe
                (\_ -> onClick CommitAspectInProgress)
                (Attr.class "no-aspect-in-progress")

        providedValue =
            ifAspectForMe
                (\e -> Attr.value aspect.name)
                (Attr.value "")

        notEditing =
            div [ Attr.class "aspect" ]
                [ kindEditSpan
                , tagSpan
                , span [ Attr.class "aspect-text" ] [ text name ]
                , a [ Attr.class "aspect-button", onClickX ] [ Icon.view Icon.x ]
                ]

        withEditing =
            div [ Attr.class "aspect", Attr.class "aspect-input" ]
                [ kindEditSpan
                , input [ Attr.class "aspect-text", Attr.placeholder "Add Aspect...", inHandler, providedValue ] []
                , a [ Attr.class "aspect-button", onClickCheck ] [ Icon.view Icon.check ]
                ]
    in
    ifAspectForMe
        (\e -> withEditing)
        notEditing



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
        (D.maybe (D.field "tags" D.int)
            |> D.andThen (Maybe.withDefault 0 >> D.succeed)
        )


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
        (D.maybe (D.field "is_pc" D.bool)
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
