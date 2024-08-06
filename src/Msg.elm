module Msg exposing (Msg(..))
import Model exposing (..)
import Http


type Msg
    = AddAspect EntityName AspectName
    | TagAspect EntityName AspectName
    | UntagAspect EntityName AspectName
    | RemoveAspect EntityName AspectName
    | UseStressBox EntityName StressTrackName Int
    | FreeStressBox EntityName StressTrackName Int
    | SetFP EntityName Int
    | IncrementFP EntityName
    | DecrementFP EntityName
    | GotGameData (Result Http.Error GameModel)
    | GotCmdReply (Result Http.Error Bool)
    | HoverFP EntityName
    | NoHoverFP
    | EditAspect EntityName AspectKind String
    | CommitAspectInProgress
    | OpenEditAspectKind
