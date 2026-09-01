module OpenAI.Import.Raw (
    addFresh, updateKnown
  ) where

import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Hasql.Pool as Hp

import qualified OpenAI.Import.Lookup as Lk
import qualified OpenAI.Import.Report as Rp
import qualified OpenAI.Import.Types as It
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Json.V2 as Jv2
import qualified OpenAI.Serialize.Conversation as Scv
import qualified OpenAI.Serialize.IncrUpdate as Sin


addFresh :: Hp.Pool -> Jd.Conversation -> IO (Either Hp.UsageError (Either Text Rp.Report))
addFresh pool conversation = do
  result <- Scv.addConversationR pool conversation
  pure $
    case result of
      Left usageError -> Left usageError
      Right (Left errorText) -> Right $ Left $ T.pack errorText
      Right (Right reportRaw) -> Right $ Right $ reportFresh conversation reportRaw


updateKnown :: Hp.Pool -> Lk.RowConv -> Jd.Conversation -> Text -> IO (Either Hp.UsageError (Either Text Rp.Report))
updateKnown pool rowConv conversation sourceKey = do
  result <- Sin.updateConversation pool conversation sourceKey
  pure $ case result of
    Left usageError -> Left usageError
    Right (Left errorText) -> Right . Left $ errorText
    Right (Right reportRaw) -> Right . Right $ reportKnown rowConv conversation reportRaw


reportFresh :: Jd.Conversation -> Scv.ReportRawAdd -> Rp.Report
reportFresh conversation reportRaw =
  Rp.Report {
    Rp.eidConv = conversation.convIdCv
    , Rp.uidConv = Just reportRaw.uidConv
    , Rp.uidDisc = Nothing
    , Rp.action = It.AddFreshA
    , Rp.count = countFresh
    , Rp.notes = notesFresh reportRaw
  }


countFresh :: Rp.Count
countFresh =
  Rp.Count {
    Rp.convAddedCnt = 1
    , Rp.convUpdatedCnt = 0
    , Rp.discAddedCnt = 0
    , Rp.discUpdatedCnt = 0
    , Rp.sumAddedCnt = 0
    , Rp.skipCnt = 0
    , Rp.failCnt = 0
  }


notesFresh :: Scv.ReportRawAdd -> [Rp.Note]
notesFresh reportRaw =
  Rp.InfoN "raw conversation added"
    : catMaybes [
        countNote "raw nodes added" reportRaw.nodeAddedCnt
        , countNote "raw messages added" reportRaw.msgAddedCnt
      ]


reportKnown :: Lk.RowConv -> Jd.Conversation -> Sin.ReportRaw -> Rp.Report
reportKnown rowConv conversation reportRaw =
  Rp.Report {
    Rp.eidConv = conversation.convIdCv
    , Rp.uidConv = Just reportRaw.uidConv
    , Rp.uidDisc = Nothing
    , Rp.action = It.UpdateKnownA
    , Rp.count = countKnown reportRaw
    , Rp.notes = notesKnown rowConv conversation reportRaw
  }


countKnown :: Sin.ReportRaw -> Rp.Count
countKnown reportRaw =
  Rp.Count {
    Rp.convAddedCnt = 0
    , Rp.convUpdatedCnt = if changedRaw reportRaw then 1 else 0
    , Rp.discAddedCnt = 0
    , Rp.discUpdatedCnt = 0
    , Rp.sumAddedCnt = 0
    , Rp.skipCnt = 0
    , Rp.failCnt = 0
  }


notesKnown :: Lk.RowConv -> Jd.Conversation -> Sin.ReportRaw -> [Rp.Note]
notesKnown rowConv conversation reportRaw =
  stateNotes <> changeNotes <> consistencyNotes <> identityNotes <> map Rp.InfoN reportRaw.notesRaw
  where
    stateNotes
      | changedRaw reportRaw = [Rp.InfoN "raw conversation updated"]
      | otherwise = [Rp.InfoN "raw conversation unchanged; identical import"]

    changeNotes =
      catMaybes [
          titleNote rowConv conversation reportRaw
          , countNote "raw nodes added" reportRaw.nodeAddedCnt
          , countNote "raw nodes moved" reportRaw.nodeMovedCnt
          , countNote "raw nodes rewritten" reportRaw.nodeRewrittenCnt
          , countNote "raw messages added" reportRaw.msgAddedCnt
          , countNote "raw messages rewritten" reportRaw.msgRewrittenCnt
        ]

    consistencyNotes
      | reportRaw.sameRaw && hasReportedWrite reportRaw =
          [Rp.WarnN "raw update report marked the conversation unchanged despite reporting semantic writes"]
      | otherwise =
          []

    identityNotes
      | rowConv.uidConv /= reportRaw.uidConv =
          [Rp.WarnN $ "raw update returned conversation uid "
            <> showText reportRaw.uidConv <> ", expected " <> showText rowConv.uidConv]
      | rowConv.eidConv /= conversation.convIdCv =
          [Rp.WarnN $ "raw update input eid " <> quoteText conversation.convIdCv
            <> " differs from the selected database eid " <> quoteText rowConv.eidConv]
      | otherwise =
          []


titleNote :: Lk.RowConv -> Jd.Conversation -> Sin.ReportRaw -> Maybe Rp.Note
titleNote rowConv conversation reportRaw
  | reportRaw.titleChanged =
      Just $ Rp.InfoN $ "raw title updated from " <> quoteText rowConv.titleConv
        <> " to " <> quoteText conversation.titleCv
  | otherwise =
      Nothing


changedRaw :: Sin.ReportRaw -> Bool
changedRaw reportRaw =
  not reportRaw.sameRaw || hasReportedWrite reportRaw


hasReportedWrite :: Sin.ReportRaw -> Bool
hasReportedWrite reportRaw =
  reportRaw.titleChanged
    || reportRaw.nodeAddedCnt > 0
    || reportRaw.nodeMovedCnt > 0
    || reportRaw.nodeRewrittenCnt > 0
    || reportRaw.msgAddedCnt > 0
    || reportRaw.msgRewrittenCnt > 0


countNote :: Text -> Int -> Maybe Rp.Note
countNote label count
  | count > 0 = Just $ Rp.InfoN $ label <> ": +" <> showText count
  | otherwise = Nothing


quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""


showText :: Show a => a -> Text
showText = T.pack . show