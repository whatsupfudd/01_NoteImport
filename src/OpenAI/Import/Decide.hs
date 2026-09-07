module OpenAI.Import.Decide
  ( classify
  , choose
  , validate
  ) where

import qualified Data.Map.Strict as Mp
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T

import OpenAI.Import.Lookup (RowConv(..))
import OpenAI.Import.Types (Action(..), ConvState(..), Opts (..), Policy (..))
import qualified OpenAI.Conversation.Json.Schema as Jd
import qualified OpenAI.Conversation.Json.Node.Order as Oor
import OpenAI.Utils (safeScientific)


validate :: Jd.Conversation -> Either [Text] Text
validate conv =
  case errs of
    [] -> Right conv.oaiIdCv
    _ -> Left errs
  where
    errs = eidIssues conv
        -- <> timeIssues conv
        -- <> mappingIssues conv

classify :: Maybe RowConv -> Jd.Conversation -> Either Text ConvState
classify mbRow conv =
  case mbRow of
    Nothing -> Right $ AbsentCS conv.oaiIdCv
    Just row ->
      if conv.updateTimeCv < row.timeUpdateCv then
        Right $ OlderCS row.uidConv row.eidConv row.timeUpdateCv conv.updateTimeCv
      else
        Right $ PresentCS row.uidConv row.eidConv row.titleConv row.timeUpdateCv


choose :: Opts -> ConvState -> Jd.Conversation -> Action
choose opts state _ =
  case state of
    AbsentCS {} -> AddFreshA
    PresentCS {} -> UpdateKnownA
    OlderCS {}
      | opts.policyOpt.allowOlderPol -> UpdateKnownA
      | otherwise -> SkipOlderA
    BrokenCS {} -> FailA

eidIssues :: Jd.Conversation -> [Text]
eidIssues conv =
  ["conversation_id is empty" | T.null (T.strip conv.oaiIdCv)]

{-
Deprecated with V2 conversations.
timeIssues :: Jd.Conversation -> [Text]
timeIssues conv =
  issueTime "create_time" conv.createTimeCv <> issueTime "update_time" conv.updateTimeCv
-}

{-
mappingIssues :: Jd.Conversation -> [Text]
mappingIssues conv
  | Mp.null conv.mappingCv = ["conversation mapping is empty"]
  | otherwise =
      case Oor.buildNodeOrd conv.mappingCv of
        Right ords
          | null ords -> ["conversation mapping did not yield any reachable nodes"]
          | otherwise -> []
        Left issues ->
          let fatalIssues = filter isFatalOI issues
          in map renderOI fatalIssues
-}

issueTime :: Text -> Double -> [Text]
issueTime label timeD
  | isNaN timeD = ["conversation " <> label <> ": time is not a number (" <> T.pack (show timeD) <> ")"]
  | isInfinite timeD = ["conversation " <> label <> ": time is infinite. "]
  | otherwise  = []


isFatalOI :: Oor.OrdIssue -> Bool
isFatalOI issue =
  case issue of
    Oor.BranchOI{} -> False
    _ -> True

renderOI :: Oor.OrdIssue -> Text
renderOI issue =
  case issue of
    Oor.MissingRootOI ->
      "conversation mapping has no root node"
    Oor.MissingNodeOI eidNode ->
      "conversation mapping refers to missing node " <> eidNode
    Oor.MissingParentOI eidNode eidParent ->
      "conversation node " <> eidNode <> " refers to missing parent " <> eidParent
    Oor.CycleOI eidNode ->
      "conversation mapping contains a cycle at node " <> eidNode
    Oor.DuplicateChildOI eidParent eidChild ->
      "conversation parent " <> eidParent <> " contains duplicate child " <> eidChild
    Oor.BranchOI eidParent childEids ->
      "conversation parent " <> eidParent <> " has multiple children "
        <> T.intercalate ", " childEids