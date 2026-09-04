module OpenAI.Conversation.Json.V1.Convert where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Mp
import Data.Text (Text) 

import qualified OpenAI.Conversation.Json.V1.Schema as Jv1
import qualified OpenAI.Conversation.Json.Schema as J
import qualified OpenAI.Conversation.Json.Types as Jt

v1ToCurrent :: Jv1.Conversation -> J.Conversation
v1ToCurrent v1 =
  let
    messages = v1NodesToMessages v1.mappingCv
    rezConv = mkDefault
  in
  rezConv {
    J.titleCv = v1.titleCv
  , J.createTimeCv = v1.createTimeCv
  , J.updateTimeCv = v1.updateTimeCv
  , J.oaiIdCv = v1.convIdCv
  , J.messagesCv = messages
  }


v1NodesToMessages :: Mp.Map Text Jv1.Node -> [J.Message]
v1NodesToMessages nodes =
  -- TODO.
  []


mkDefault :: J.Conversation
mkDefault =
  J.Conversation {
    J.versionJsonCv = Jt.V1vj
  , J.titleCv = ""
  , J.createTimeCv = 0
  , J.updateTimeCv = 0
  , J.oaiIdCv = ""
  , J.moderationResultsCv = []
  , J.pluginIdsCv = Nothing
  , J.templateIdCv = ""
  , J.gizmoIdCv = ""
  , J.gizmoTypeCv = ""
  , J.isArchivedCv = False
  , J.isStarredCv = Nothing
  , J.safeUrlsCv = []
  , J.blockedUrlsCv = []
  , J.defaultModelSlugCv = ""
  , J.atlasModeEnabledCv = Nothing
  , J.conversationOriginCv = Nothing
  , J.isReadOnlyCv = Nothing
  , J.voiceCv = Nothing
  , J.asyncStatusCv = Nothing
  , J.disabledToolIdsCv = []
  , J.isTemporaryChatCv = False
  , J.isDoNotRememberCv = False
  , J.memoryScopeCv = ""
  , J.contextScopesCv = []
  , J.sugarItemIdCv = Nothing
  , J.sugarItemVisibleCv = False
  , J.pinnedTimeCv = Nothing
  , J.isStudyModeCv = False
  , J.ownerCv = mkDefaultOwner
  , J.messagesCv = []
  , J.currentNodeCv = ""
  , J.pageInfoCv = mkDefaultPageInfo
  , J.contextTruncationContinuationCv = Nothing
  }

mkDefaultOwner :: J.Owner
mkDefaultOwner =
  J.Owner {
    J.userIdOwner = ""
    , J.userEmailOwner = ""
    , J.nameOwner = ""
    , J.avatarUrlOwner = ""
  }

mkDefaultPageInfo :: J.PageInfo
mkDefaultPageInfo =
  J.PageInfo {
    J.startCursorPI = ""
    , J.endCursorPI = ""
    , J.hasPreviousPI = False
    , J.hasNextPagePI = False
  }