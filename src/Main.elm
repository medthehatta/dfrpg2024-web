port module Main exposing (main)

import Browser
import FontAwesome as Icon
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import FontAwesome.Transforms as Icon
import Home exposing (home)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Model exposing (..)
import Msg exposing (Msg(..))
import VitePluginHelper


initialUmbra =
    { name = "Umbra"
    , portrait = "whatever.png"
    , stresses =
        [ { name = "Physical", cap = 3, used = [] }
        , { name = "Mental", cap = 3, used = [ 2 ] }
        , { name = "Hunger", cap = 2, used = [ 1 ] }
        ]
    , fate = { refresh = 4, available = 5 }
    , aspects =
        [ { name = "Rekt", kind = Consequence Moderate, tags = 0 }
        , { name = "Dizzy", kind = Consequence Mild, tags = 0 }
        , { name = "Super Rekt", kind = Consequence Severe, tags = 0 }
        , { name = "Generic", kind = Generic, tags = 2 }
        , { name = "Sticky", kind = Sticky, tags = 1 }
        , { name = "Fragile", kind = Fragile, tags = 0 }
        ]
    }


initialScene =
    { name = "Scene"
    , portrait = "whatever.png"
    , stresses = []
    , fate = { refresh = 0, available = 0 }
    , aspects =
        [ { name = "Sticky", kind = Sticky, tags = 0 }
        , { name = "Fragile", kind = Fragile, tags = 0 }
        ]
    }


initialEntities =
    [ initialScene
    , initialUmbra
    ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { entities = initialEntities} , Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
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
