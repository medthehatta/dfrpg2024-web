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
    | Style


type alias StressTrack =
    { name : StressTrackName, cap : Int, used : List Int }


type alias Entity =
    { name : EntityName
    , portrait : String
    , stresses : List StressTrack
    , fate : { refresh : Int, available : Int }
    , aspects : List Aspect
    , isPc : Bool
    }


type alias TurnOrder =
    { orderedNames : List String
    , deferredNames : List String
    , current : Maybe Int
    }


type alias GameModel =
    { entities : List Entity
    , order : TurnOrder
    }


type alias AspectInProgress =
    { entity : EntityName
    , kind : AspectKind
    , name : String
    }


type alias Model =
    { game : GameModel
    , edit : EditInProgress
    , error : HasError
    }


type HasError =
    NotHasError
    | HasError String


type EditInProgress
    = NotEditing
    | EditingFatePoints EntityName
    | EditingAspectString EntityName AspectKind String
    | EditingAspectKind EntityName AspectKind String


type CommandError
    = NoError
    | CommandError String
