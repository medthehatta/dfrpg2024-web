module Model exposing (..)


type alias EntityName =
    String


type alias AspectName =
    String


type alias StressTrackName =
    String


type alias Aspect =
    { name : AspectName
    , kind : AspectKind
    , tags : Int
    }


type Severity
    = Mild
    | Moderate
    | Severe


type AspectKind
    = Generic
    | Consequence Severity
    | Fragile
    | Sticky


type alias StressTrack =
    { name : StressTrackName, cap : Int, used : List Int }


type alias Entity =
    { name : EntityName
    , portrait : String
    , stresses : List StressTrack
    , fate : { refresh : Int, available : Int }
    , aspects : List Aspect
    }


type alias Model =
    { entities : List Entity
    }
