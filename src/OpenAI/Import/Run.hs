module OpenAI.Import.Run
  ( runOne
  , planOne
  )
where

import Control.Applicative ((<|>))
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Hasql.Pool as Hp
import qualified Network.HTTP.Client as Ht
import qualified OpenAI.Import.Decide as Id
import qualified OpenAI.Import.Discussion as Ids
import qualified OpenAI.Import.Lookup as Il
import qualified OpenAI.Import.Raw as Ir
import qualified OpenAI.Import.Report as Ire
import qualified OpenAI.Import.Summary as Is
import qualified OpenAI.Import.Types as It
import qualified OpenAI.Json.Reader as Jd

planOne
  :: It.Opts
  -> Maybe Il.RowConv
  -> Jd.Conversation
  -> Either Ire.Report (It.ConvState, It.Action)
planOne opts mbRow conv =
  case Id.validate conv of
    Left issues ->
      Left $
        Ire.Report
          { eidConv = conv.convIdCv
          , uidConv = Nothing
          , uidDisc = Nothing
          , action = It.FailA
          , count = Ire.emptyCount { Ire.failCnt = 1 }
          , notes = fmap Ire.WarnN issues
          }
    Right _ ->
      let state = Id.classify mbRow conv
          act = Id.choose opts state conv
      in Right (state, act)

runOne
  :: Maybe Ht.Manager
  -> Hp.Pool
  -> It.Source
  -> It.Opts
  -> Jd.Conversation
  -> IO (Either Hp.UsageError Ire.Report)
runOne mgr pool _src opts conv = do
  mbRowE <- Il.byEid pool conv.convIdCv
  case mbRowE of
    Left dbErr ->
      pure $ Left dbErr
    Right mbRow ->
      case planOne opts mbRow conv of
        Left report ->
          pure $ Right report
        Right (state, act) ->
          case opts.modeOpt of
            It.DryM ->
              pure $ Right $ dryReport mbRow conv state act
            It.WriteM ->
              runWrite mgr pool opts mbRow conv state act

runWrite
  :: Maybe Ht.Manager
  -> Hp.Pool
  -> It.Opts
  -> Maybe Il.RowConv
  -> Jd.Conversation
  -> It.ConvState
  -> It.Action
  -> IO (Either Hp.UsageError Ire.Report)
runWrite mgr pool opts mbRow conv state act =
  case act of
    It.AddFreshA -> do
      rawE <- Ir.addFresh pool conv
      finish mgr pool opts Nothing conv rawE

    It.UpdateKnownA ->
      case (mbRow, state) of
        (Just row, It.PresentCS {}) -> do
          rawE <- Ir.updateKnown pool row conv
          finish mgr pool opts (Just row.uidConv) conv rawE

        (Just row, It.OlderCS {})
          | opts.policyOpt.allowOlderPol -> do
              rawE <- Ir.updateKnown pool row conv
              finish mgr pool opts (Just row.uidConv) conv rawE

        _ ->
          pure $ Right $ failReport (uidFromState state <|> uidFromRow mbRow) conv "invalid update state"

    It.SkipOlderA ->
      pure $ Right $ skipReport (uidFromState state <|> uidFromRow mbRow) conv state

    It.SkipSameA ->
      pure $ Right $ sameReport (uidFromState state <|> uidFromRow mbRow) conv state

    It.FailA ->
      pure $ Right $ failReport (uidFromState state <|> uidFromRow mbRow) conv (failText state)

finish
  :: Maybe Ht.Manager
  -> Hp.Pool
  -> It.Opts
  -> Maybe Int64
  -> Jd.Conversation
  -> Either Hp.UsageError (Either T.Text Ire.Report)
  -> IO (Either Hp.UsageError Ire.Report)
finish mgr pool opts uidMb conv rawE =
  case rawE of
    Left dbErr ->
      pure $ Left dbErr

    Right (Left err) ->
      pure $ Right $ errorReport uidMb conv err

    Right (Right report0) -> do
      discE <- Ids.sync pool opts conv.convIdCv report0
      case discE of
        Left dbErr ->
          pure $ Left dbErr

        Right (Left err) ->
          pure $ Right $ addNote (Ire.WarnN err) report0

        Right (Right report1) -> do
          sumE <- Is.sync mgr pool opts conv.convIdCv report1
          case sumE of
            Left dbErr ->
              pure $ Left dbErr

            Right (Left err) ->
              pure $ Right $ addNote (Ire.WarnN err) report1

            Right (Right report2) ->
              pure $ Right report2

dryReport :: Maybe Il.RowConv -> Jd.Conversation -> It.ConvState -> It.Action -> Ire.Report
dryReport mbRow conv state act =
  baseReport
    (uidFromState state <|> uidFromRow mbRow)
    conv.convIdCv
    act
    (countFor act)
    (Ire.InfoN "dry-run" : notesDry state act)

skipReport :: Maybe Int64 -> Jd.Conversation -> It.ConvState -> Ire.Report
skipReport uidMb conv state =
  baseReport uidMb conv.convIdCv It.SkipOlderA (countFor It.SkipOlderA) (notesSkip state)

sameReport :: Maybe Int64 -> Jd.Conversation -> It.ConvState -> Ire.Report
sameReport uidMb conv state =
  baseReport uidMb conv.convIdCv It.SkipSameA (countFor It.SkipSameA) (notesSame state)

failReport :: Maybe Int64 -> Jd.Conversation -> T.Text -> Ire.Report
failReport uidMb conv msg =
  baseReport uidMb conv.convIdCv It.FailA (countFor It.FailA) [Ire.ErrorN msg]

errorReport :: Maybe Int64 -> Jd.Conversation -> T.Text -> Ire.Report
errorReport uidMb conv msg =
  baseReport uidMb conv.convIdCv It.FailA (countFor It.FailA) [Ire.ErrorN msg]

baseReport :: Maybe Int64 -> T.Text -> It.Action -> Ire.Count -> [Ire.Note] -> Ire.Report
baseReport uidMb eid act cnt notes =
  Ire.Report
    { eidConv = eid
    , uidConv = uidMb
    , uidDisc = Nothing
    , action = act
    , count = cnt
    , notes = notes
    }

addNote :: Ire.Note -> Ire.Report -> Ire.Report
addNote note report =
  report { Ire.notes = report.notes <> [note] }

countFor :: It.Action -> Ire.Count
countFor act =
  case act of
    It.AddFreshA ->
      Ire.emptyCount { Ire.convAddedCnt = 1 }

    It.UpdateKnownA ->
      Ire.emptyCount { Ire.convUpdatedCnt = 1 }

    It.SkipOlderA ->
      Ire.emptyCount { Ire.skipCnt = 1 }

    It.SkipSameA ->
      Ire.emptyCount { Ire.skipCnt = 1 }

    It.FailA ->
      Ire.emptyCount { Ire.failCnt = 1 }

uidFromRow :: Maybe Il.RowConv -> Maybe Int64
uidFromRow mbRow =
  case mbRow of
    Nothing -> Nothing
    Just row -> Just row.uidConv

uidFromState :: It.ConvState -> Maybe Int64
uidFromState state =
  case state of
    It.PresentCS {} -> Just state.uidConv
    It.OlderCS {} -> Just state.uidConv
    _ -> Nothing

notesDry :: It.ConvState -> It.Action -> [Ire.Note]
notesDry state act =
  case act of
    It.AddFreshA ->
      [Ire.InfoN "would add fresh conversation"]

    It.UpdateKnownA ->
      case state of
        It.OlderCS {} ->
          [ Ire.WarnN $
              "would update existing conversation from older export because policy allows it"
                <> " (db="
                <> showT state.timeUpdateDb
                <> ", json="
                <> showT state.timeUpdateJs
                <> ")"
          ]
        _ ->
          [Ire.InfoN "would update existing conversation"]

    It.SkipOlderA ->
      notesSkip state

    It.SkipSameA ->
      notesSame state

    It.FailA ->
      notesFail state

notesSkip :: It.ConvState -> [Ire.Note]
notesSkip state =
  case state of
    It.OlderCS {} ->
      [ Ire.WarnN $
          "older export skipped"
            <> " (db="
            <> showT state.timeUpdateDb
            <> ", json="
            <> showT state.timeUpdateJs
            <> ")"
      ]

    _ ->
      [Ire.InfoN "conversation skipped"]

notesSame :: It.ConvState -> [Ire.Note]
notesSame _ =
  [Ire.InfoN "conversation unchanged"]

notesFail :: It.ConvState -> [Ire.Note]
notesFail state =
  case state of
    It.BrokenCS {} ->
      if null state.issues
        then [Ire.ErrorN "invalid conversation"]
        else fmap Ire.ErrorN state.issues

    _ ->
      [Ire.ErrorN "invalid conversation"]

failText :: It.ConvState -> T.Text
failText state =
  case state of
    It.BrokenCS {} ->
      if null state.issues
        then "invalid conversation"
        else "invalid conversation: " <> T.intercalate "; " state.issues

    _ ->
      "invalid conversation"

showT :: Show a => a -> T.Text
showT =
  T.pack . show