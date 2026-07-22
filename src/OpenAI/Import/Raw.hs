module OpenAI.Import.Raw
  ( addFresh
  , updateKnown
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
import qualified OpenAI.Serialize.Conversation as Scv
import qualified OpenAI.Serialize.IncrUpdate as Sin

addFresh
  :: Hp.Pool
  -> Jd.Conversation
  -> IO (Either Hp.UsageError (Either Text Rp.Report))
addFresh pool conv = do
  rez <- Scv.addConversation pool conv
  pure $ case rez of
    Left err ->
      Left err
    Right (Left err) ->
      Right $ Left (textErr err)
    Right (Right uid) ->
      Right $ Right (reportFresh conv uid)

updateKnown
  :: Hp.Pool
  -> Lk.RowConv
  -> Jd.Conversation
  -> IO (Either Hp.UsageError (Either Text Rp.Report))
updateKnown pool row conv = do
  rez <- Sin.updateConversation pool conv
  pure $ case rez of
    Left err ->
      Left err
    Right (Left err) ->
      Right $ Left (textErr err)
    Right (Right raw) ->
      Right $ Right (reportKnown row conv raw)

reportFresh :: Jd.Conversation -> Int64 -> Rp.Report
reportFresh conv uid =
  Rp.Report
    { eidConv = conv.convIdCv
    , uidConv = Just uid
    , uidDisc = Nothing
    , action = It.AddFreshA
    , count = Rp.emptyCount { Rp.convAddedCnt = 1 }
    , notes =
        [ Rp.InfoN "raw conversation added"
        ]
    }

reportKnown :: Lk.RowConv -> Jd.Conversation -> Sin.ReportRaw -> Rp.Report
reportKnown row conv raw =
  Rp.Report
    { eidConv = conv.convIdCv
    , uidConv = Just raw.uidConv
    , uidDisc = Nothing
    , action = It.UpdateKnownA
    , count = Rp.emptyCount { Rp.convUpdatedCnt = if raw.sameRaw then 0 else 1 }
    , notes = notesKnown row conv raw
    }

notesKnown :: Lk.RowConv -> Jd.Conversation -> Sin.ReportRaw -> [Rp.Note]
notesKnown row conv raw =
  let titleChanged = row.titleConv /= conv.titleCv
      timeChanged = row.timeUpdateCv /= conv.updateTimeCv
      metaChanged = titleChanged || timeChanged
      nodesAdded = raw.nodeAddedCnt
      msgsAdded = raw.msgAddedCnt
      structChanged = nodesAdded > 0 || msgsAdded > 0

      infoBase
        | raw.sameRaw =
            [Rp.InfoN "raw conversation unchanged"]
        | structChanged =
            [Rp.InfoN ("raw conversation updated: " <> renderStruct nodesAdded msgsAdded)]
        | metaChanged =
            [Rp.InfoN "raw conversation metadata updated"]
        | otherwise =
            [Rp.InfoN "raw conversation updated"]

      infoMeta
        | raw.sameRaw = []
        | structChanged && metaChanged = [Rp.InfoN "raw conversation metadata updated"]
        | otherwise = []

      warns =
        concat
          [ [ Rp.WarnN
                ( "raw updater returned uid "
                    <> showT raw.uidConv
                    <> " but lookup row was uid "
                    <> showT row.uidConv
                )
            | raw.uidConv /= row.uidConv
            ]
          , [ Rp.WarnN
                ( "raw updater processed eid "
                    <> conv.convIdCv
                    <> " but lookup row was for eid "
                    <> row.eidConv
                )
            | conv.convIdCv /= row.eidConv
            ]
          , [ Rp.WarnN "raw updater reported unchanged state although title or update_time differ"
            | raw.sameRaw && metaChanged
            ]
          ]

   in infoBase <> infoMeta <> warns

renderStruct :: Int -> Int -> Text
renderStruct nodeCnt msgCnt =
  case catMaybes [plusTxt "nodes" nodeCnt, plusTxt "messages" msgCnt] of
    [] ->
      "no structural additions"
    xs ->
      T.intercalate ", " xs

plusTxt :: Text -> Int -> Maybe Text
plusTxt label n
  | n <= 0 = Nothing
  | otherwise = Just (label <> " +" <> showT n)

textErr :: String -> Text
textErr = T.pack

showT :: Show a => a -> Text
showT = T.pack . show