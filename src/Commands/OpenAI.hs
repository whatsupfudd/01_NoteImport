{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Commands.OpenAI where

import qualified Control.Monad.Cont as Mc
import Control.Monad (unless)

import qualified Data.ByteString.Lazy as BS
import Data.Either (lefts, rights)
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import qualified Data.Set as Set
import Data.Text (Text, unpack, intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.Vector as V

import System.FilePath ((</>))

import qualified Data.Aeson as Ae

import qualified Hasql.Pool as Hp
import qualified Network.HTTP.Client as Ht

import qualified Options.Runtime as Rto
import qualified Options.Types as Opt
import qualified DB.Connect as Dbc
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Conversation as Cv
import qualified OpenAI.Parse as Op
import qualified OpenAI.Serialize.Conversation as Scv
import qualified OpenAI.Deserialize.Conversation as Dcv
import qualified OpenAI.Serialize.Discussion as Sdc
import qualified OpenAI.Deserialize.Discussion as Ddc
import qualified OpenAI.Generate.Docx as Gd
import qualified OpenAI.Generate.DocxDb as Gdb
import qualified OpenAI.Summarisation as Sm
import qualified OpenAI.Generate.Elmify as Em
import qualified OpenAI.ProjFetcher as Pf
import qualified OpenAI.ConvToDisc as Ccv
import qualified OpenAI.Utils as Utl


oaiCmd :: Opt.OaiSubCommand -> Rto.RunOptions -> IO ()
oaiCmd opts rtOpts =
  case opts of
    Opt.JsonSC oaiOpts -> case oaiOpts of
      Opt.PrintJS jsonOpts targetsOpts -> printJson jsonOpts targetsOpts
      Opt.StoreJS jsonOpts targetsOpts -> storeJsonAsConversations jsonOpts targetsOpts rtOpts
    Opt.SummarySC oaiSummaryOpts -> saveSummaries oaiSummaryOpts rtOpts
    Opt.DocxSC genOpts -> saveDocx genOpts rtOpts
    Opt.ElmSC genOpts -> saveElmFiles genOpts rtOpts
    Opt.ProjFetchSC oaiProjFetchOpts -> saveProject oaiProjFetchOpts rtOpts
    Opt.ConversationSC conversationOpts -> case conversationOpts of
      Opt.DeserializeCS oaiGenOpts -> deserializeConversation oaiGenOpts rtOpts
      Opt.ConvertCS oaiTargetsOpts -> convertConversation oaiTargetsOpts rtOpts
      Opt.DocxCS oaiGenOpts -> saveConversationToDocx oaiGenOpts rtOpts


data ItemIdent =
  EidCI Text
  | UidCI Int64
  | UuidCI UUID
  deriving (Show)


data GfTarget = GfTarget {
  fileIdGF :: Text
  , titleGF :: Text
  , uidGF :: Text
} deriving (Show)


gfTargets :: [GfTarget]
gfTargets =
  let
    rawEntries =[
        ("D1", "Sukuk and Takaful feasibility", "69089c63-47cc-832c-8e01-1a11b8da36aa")
      , ("D2", "GoldFann banking strategy draft", "6905da75-e670-832f-89bc-05d843b67a26")
      , ("D3", "Blockchain transaction strategy", "6906717d-8c9c-8327-8f08-9e9df05e3b0e")
      , ("D4", "SoW for RfP (v2)", "68f32971-b760-8330-a9e8-6e9e3f3d53ce")
      , ("D5", "Classification and Tax risk", "68e7474c-a644-8327-83f6-e733361a5c7c")
      , ("D6", "GoldFann SoW strategy", "68ebc9e2-cf94-8332-9f90-0bfa44ae1346")
      , ("D7", "SV ecosystem strategy", "68e0b4ea-cf00-8330-995a-7063bbfe5dce")
      , ("D8", "UAE vs Switzerland comparison", "68d6cf94-3c04-832c-be43-f356c3ce7c06")
      , ("D9", "SV structure, counsel, issues review", "68c931a8-5064-8329-a437-70b1954fbad5")
      , ("D10", "AML KYC outsourcing responsibilities", "68d146a0-6bb4-8333-bfd6-c6a5202e9929")
      , ("D11", "GoldFann RoI analysis", "68b9a4ba-7870-832f-b33f-aa40e86eceec")
      , ("D12", "Luxembourg Token Fund Strategy", "68559736-c58c-8011-b3d1-9b75d90318de")
      , ("D13", "UCITS vs SV comparison", "68a6b092-4df8-832c-b795-2ca7e5d99ae6")
      , ("D14", " (v2) Lux structuring strategy", "68a59f2a-9458-8324-9697-ac460adfc1d3")
      , ("D15", "Insurance Strategy for ArtCrypto", "6864d0e7-8574-8011-a5ee-a79ba92f1453")
      , ("D16", "Gold Bullion Title Deeds Strategy", "6805cf5d-2d24-8011-a374-bb4bfaf8bb41")
      , ("D17", "Cardano Blockchain for REIT", "681fa5b2-0aa0-8011-954b-dc381755168d")
      ]
  in
  map (\(fileId, title, uid) -> GfTarget fileId title uid) rawEntries


parseJson :: FilePath -> Bool -> IO (Either String [Jd.Conversation])
parseJson jsonFile exportB = do
  jsonContent <- BS.readFile jsonFile
  pure $ if exportB then
    Ae.eitherDecode jsonContent :: Either String [Jd.Conversation]
  else
    L.singleton <$> (Ae.eitherDecode jsonContent :: Either String Jd.Conversation)


printJson :: Opt.OaiJsonOpts -> Opt.TargetsOpts -> IO ()
printJson jsonOpts targetsOpts = do
  rezA <- parseJson jsonOpts.jsonFile jsonOpts.exportB
  case rezA of
    Left err -> putStrLn $ "Parsing failed: " ++ err
    Right conversations ->
      case targetsOpts.targetsTO of
        [] -> mapM_ showConversation conversations
        targets ->
          let
            orgMap = Mp.fromList $ map (\conversation -> (conversation.convIdCv, conversation)) conversations
            targetConvrs = Mp.restrictKeys orgMap (Set.fromList targets)
          in do
          mapM_ showConversation targetConvrs


storeJsonAsConversations :: Opt.OaiJsonOpts -> Opt.TargetsOpts -> Rto.RunOptions -> IO ()
storeJsonAsConversations jsonOpts targetsOpts rtOpts = do
  rezA <- parseJson jsonOpts.jsonFile jsonOpts.exportB
  case rezA of
    Left err -> putStrLn $ "Parsing failed: " ++ err
    Right conversations ->
      let
        pgPool = Dbc.startPg rtOpts.pgDbConf
        targetConv = case targetsOpts.targetsTO of
          [] -> conversations
          targets ->
            let
              orgMap = Mp.fromList $ map (\conversation -> (conversation.convIdCv, conversation)) conversations
              targetConvrs = Mp.restrictKeys orgMap (Set.fromList targets)
            in
            Mp.elems targetConvrs
      in do
      rezA <- Mc.runContT pgPool (saveConversations targetConv)
      reportDbErrors "storeJsonAsConversations" rezA


reportDbErrors :: String -> Either [Hp.UsageError] (Either [String] [resultT]) -> IO ()
reportDbErrors opLabel eiRez =
  let
    opPrefix = "@[" <> opLabel <> "] "
  in
  case eiRez of
    Left errs -> putStrLn $ opPrefix <> "db err: " <> show errs
    Right (Left errs) -> putStrLn $ opPrefix <> "logic err: " <> show errs
    Right (Right successInResults) -> putStrLn $ opPrefix <> "saved " <> show (length successInResults) <> "."


saveSummaries :: Opt.TargetsOpts -> Rto.RunOptions -> IO ()
saveSummaries oaiSummaryOpts rtOpts =
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in do
  rezA <- Mc.runContT pgPool (genSummaries oaiSummaryOpts)
  reportDbErrors "saveSummaries" rezA


saveElmFiles :: Opt.OaiGenOpts -> Rto.RunOptions -> IO ()
saveElmFiles genOpts rtOpts =
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in do
  -- rezA <- Mc.runContT pgPool (generateElmFiles genOpts.destPath gfTargets)
  --reportDbErrors "saveElmFiles" rezA
  rezA <- Mc.runContT pgPool (doOnTargets genOpts generateElmFromItem)
  reportDbErrors "saveElmFiles" rezA

type OperFunction resultT = Hp.Pool -> FilePath -> (Int, ItemIdent) -> IO (Either Hp.UsageError (Either String resultT))

doOnTargets :: Opt.OaiGenOpts -> OperFunction resultT -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [resultT]))
doOnTargets genOpts operation pgPool =
  case genOpts.group of
    Nothing ->
      case genOpts.targets of
        [] -> pure . Right $ Left ["@[doOnTargets] no targets to save."]
        someTargets ->
          let
            targets = map EidCI someTargets
          in do
          putStrLn $ "@[doOnTargets] targets: " <> show someTargets <> ", saving " <> show (length targets) <> " targets."
          rezA <- mapM (operation pgPool genOpts.destPath) (zip [1..] targets)
          pure $ Utl.listResultsToResultList rezA
    Just aGroup -> do
      eiDiscussions <- Ddc.allDiscussionsInGroup pgPool aGroup
      case eiDiscussions of
        Left err -> pure . Left $ [err]
        Right discussions ->
          let
            targetIDs = V.toList $ V.map (\(uid, title, uuid) -> UuidCI uuid) discussions
          in do
            putStrLn $ "@[doOnTargets] group: " <> unpack aGroup <> ", saving " <> show (length targetIDs) <> " targets."
            rezA <- mapM (operation pgPool genOpts.destPath) (zip [1..] targetIDs)
            pure $ Utl.listResultsToResultList rezA


saveDocx :: Opt.OaiGenOpts -> Rto.RunOptions -> IO ()
saveDocx genOpts rtOpts =
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in do
  rezA <- Mc.runContT pgPool (saveDocxFromDiscs genOpts)
  reportDbErrors "saveDocx" rezA


saveProject :: Opt.OaiProjFetchOpts -> Rto.RunOptions -> IO ()
saveProject fetchOpts rtOpts = do
  projects <- Pf.saveProjects fetchOpts.sourcePath
  putStrLn $ "Group: " <> unpack fetchOpts.label <> ", Projects (" <> show (length projects) <> ")"
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  rezA <- Mc.runContT pgPool (Pf.saveDescriptionsToGroup fetchOpts.label projects)
  reportDbErrors "saveProject" rezA


deserializeConversation :: Opt.OaiGenOpts -> Rto.RunOptions -> IO ()
deserializeConversation oaiGenOpts rtOpts =
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in
  case oaiGenOpts.group of
    Just group -> do
      putStrLn $ "@[deserializeConversation] deserializing conversation: " <> oaiGenOpts.destPath <> " for group: " <> unpack group
    Nothing ->
      case oaiGenOpts.targets of
        [] -> putStrLn "@[deserializeConversation] no targets to deserialize."
        targets -> do
          rezA <- Mc.runContT pgPool (convDeserialize targets oaiGenOpts.destPath)
          reportDbErrors "deserializeConversation" rezA


convDeserialize :: [Text] -> FilePath -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Cv.ConversationDb]))
convDeserialize targets destPath pgPool = do
    results <- mapM (\target -> do
        eiErrRez <- Dcv.getConversationByEid pgPool target
        case eiErrRez of
          Left err -> pure $ Left err
          Right eiMbConv ->
            case eiMbConv of
              Left errMsg -> pure . Right $ Left errMsg
              Right mbConv -> case mbConv of
                Nothing -> pure . Right $ Left "no conversation found"
                Just convDb -> do
                  putStrLn $ "@[convertConversation] deserializing conversation: " <> destPath <> " for target: " <> unpack target
                  -- print convDb
                  case Ccv.analyzeConversation convDb of
                    Left errMsgA -> pure . Right . Left $ unpack errMsgA
                    Right context -> do
                      rezA <- Gd.writeContextDocx context convDb.titleCv (destPath </> unpack convDb.eidCv <> ".docx")
                      case rezA of
                        Left errMsgB -> do
                          putStrLn $ "@[convertConversation] error: " <> errMsgB
                          pure . Right . Left $ errMsgB
                        Right _ -> pure . Right $ Right convDb
                      -- putStrLn $ "@[deserializeConversation] analyzed conversation: " <> destPath <> " for target: " <> unpack target
                      -- print context
                      pure . Right $ Right convDb
      ) targets
    case lefts results of
      [] -> case lefts $ rights results of
        [] -> pure . Right . Right . rights . rights $ results
        errMsgs -> pure . Right . Left $ errMsgs
      errs -> pure $ Left errs


convertConversation :: Opt.TargetsOpts -> Rto.RunOptions -> IO ()
convertConversation targetOpts rtOpts =
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in
  case targetOpts.groupTO of
    Just group -> do
      putStrLn $ "@[convertConversation] group not supported yet: " <> unpack group
    Nothing ->
      case targetOpts.targetsTO of
        [] -> do
          rezA <- Mc.runContT pgPool convStoreAllConversations
          reportDbErrors "convStoreAllConversations" rezA
        targets ->
          let
            targetIDs = map EidCI targets
          in do
          rezA <- Mc.runContT pgPool (convStoreDiscussions targetIDs)
          reportDbErrors "convStoreDiscussion" rezA


convStoreAllConversations :: Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
convStoreAllConversations pgPool = do
  conversations <- Dcv.fetchAllConversations pgPool
  case conversations of
    Left err -> pure . Left $ [err]
    Right conversations ->
      let
        targetIDs = map UidCI (Mp.elems conversations)
      in do
      convStoreDiscussions targetIDs pgPool


convStoreDiscussions :: [ItemIdent] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
convStoreDiscussions targets pgPool = do
  results <- mapM (convStoreADiscussion pgPool) targets
  case lefts results of
    [] -> case lefts $ rights results of
      [] -> pure . Right . Right . rights . rights $ results
      errMsgs -> pure . Right . Left $ errMsgs
    errs -> pure $ Left errs


convStoreADiscussion :: Hp.Pool -> ItemIdent -> IO (Either Hp.UsageError (Either String Int64))
convStoreADiscussion pgPool target = do
  eiErrRez <- case target of
    EidCI eid -> Dcv.getConversationByEid pgPool eid
    UidCI uid -> Dcv.getConversationByUid pgPool uid
  case eiErrRez of
    Left err -> pure $ Left err
    Right eiMbConv ->
      case eiMbConv of
        Left errMsg -> pure . Right $ Left errMsg
        Right mbConv -> case mbConv of
          Nothing -> pure . Right $ Left "no conversation found"
          Just convDb -> do
            putStrLn $ "@[convStoreDiscussion] deserializing for target: " <> show target
            -- print convDb
            case Ccv.analyzeConversation convDb of
              Left errMsgA -> pure . Right . Left $ unpack errMsgA
              Right context -> do
                rezA <- Sdc.storeDiscussion pgPool convDb.titleCv convDb.eidCv context
                case rezA of
                  Left errMsgB -> do
                    putStrLn $ "@[convertConversation] error: " <> errMsgB
                    pure . Right . Left $ errMsgB
                  Right (ctxUid, ctxUuid) -> pure . Right $ Right ctxUid
                -- print context


saveConversations :: [Jd.Conversation] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
saveConversations discussions pgPool = do
  discussionMap <- Dcv.fetchAllConversations pgPool
  case discussionMap of
    Left err -> pure . Left $ [err]
    Right discussionMap ->
      let
        newDiscussions = filter (\discussion -> not (Mp.member discussion.titleCv discussionMap)) discussions
      in
      case newDiscussions of
        [] -> do
          putStrLn $ "@[saveConversations] no new discussions to save."
          pure . Right . Right $ []
        _ -> do
          putStrLn $ "@[saveConversations] saving " <> show (length newDiscussions) <> " new "
              <> (if length newDiscussions > 1 then "conversations" else "conversation")
              <> "."
          results <- mapM (Scv.addConversation pgPool) newDiscussions
          pure $ Utl.listResultsToResultList results


showConversation :: Jd.Conversation -> IO ()
showConversation conversation = do
  putStrLn . unpack $ "Title: " <> conversation.titleCv <> ", id: " <> conversation.convIdCv
  let
    (analysis, issues) = Op.showDiscussion conversation
  putStrLn . unpack $ analysis
  unless (null issues) $
    putStrLn $ "@[showConversation] issues: " <> L.intercalate "\n" (map unpack issues)


mapDiscussionsToElm :: [Jd.Conversation] -> IO ()
mapDiscussionsToElm discussions = do
  mapM_ (\discussion -> do
    putStrLn . unpack $ "Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv
    let
      rezElm = Op.discussionToElm discussion
    case rezElm of
      Left err -> putStrLn $ "@[mapDiscussionsToElm] error: " <> unpack err
      Right elm -> putStrLn . unpack $ elm
    ) discussions


runOnDiscussionSubset :: [Jd.Conversation] -> [GfTarget] -> (Mp.Map Text GfTarget -> Jd.Conversation -> IO ()) -> IO ()
runOnDiscussionSubset discussions targets evalFct =
  let
    targetMap = Mp.fromList $ map (\target -> (target.uidGF, target)) targets
    targetDiscussions = filter (\discussion -> Mp.member discussion.convIdCv targetMap) discussions
  in
  mapM_ (evalFct targetMap) targetDiscussions


extractGFContent :: [Jd.Conversation] -> IO ()
extractGFContent discussions =
  runOnDiscussionSubset discussions gfTargets makeElmFile
  where
  makeElmFile :: Mp.Map Text GfTarget -> Jd.Conversation -> IO ()
  makeElmFile targetMap discussion = do
    putStrLn . unpack $ "Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv
    let
      rezElm = Op.discussionToElm discussion
    case rezElm of
      Left err -> putStrLn $ "@[extractGFContent] error: " <> unpack err
      Right elm ->
        case Mp.lookup discussion.convIdCv targetMap of
          Just target ->
            let
              outPath = "/tmp/" <> unpack target.fileIdGF <> ".elm"
              elmContent = elmPreambule target <> "\n\nmessages = " <> elm <> "\n"
            in do
            Tio.writeFile outPath elmContent
            putStrLn $ "@[extractGFContent] wrote to " <> outPath
          Nothing ->
            putStrLn $ "@[extractGFContent] no target found for discussion: " <> unpack discussion.titleCv <> ", id: " <> unpack discussion.convIdCv

  elmPreambule :: GfTarget -> Text
  elmPreambule aTarget =
    let
      moduleDef = "module LegalNodes.Docs." <> aTarget.fileIdGF <> " exposing (content)\n"
      importDef = "import Components.LegalNodes.ReferenceDoc.Types as T\n"
      contentDef = "content = (\"" <> aTarget.titleGF <> "\", \"" <> aTarget.uidGF <> "\", messages)"
    in
    intercalate "\n" [
      moduleDef
      , importDef
      , contentDef
    ]


saveDocxFromDiscs :: Opt.OaiGenOpts -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
saveDocxFromDiscs genOpts pgPool = do
  case genOpts.group of
    Nothing ->
      case genOpts.targets of
        [] -> pure . Right $ Left ["@[saveDocxFromDiscs] no targets to save."]
        someTargets ->
          let
            targets = map EidCI someTargets
          in do
          rezA <- mapM (genDocxByDiscId pgPool genOpts.destPath) targets
          pure $ Utl.listResultsToResultList rezA
    Just aGroup -> do
      eiDiscussions <- Ddc.allDiscussionsInGroup pgPool aGroup
      case eiDiscussions of
        Left err -> pure . Left $ [err]
        Right discussions ->
          let
            targetIDs = V.toList $ V.map (\(uid, title, oaiid) -> UidCI uid) discussions
          in do
            rezA <- mapM (genDocxByDiscId pgPool genOpts.destPath) targetIDs
            pure $ Utl.listResultsToResultList rezA


genDocxFromConvs :: [Jd.Conversation] -> IO ()
genDocxFromConvs conversations =
  let
    subTargets = take 1 gfTargets
  in
  runOnDiscussionSubset conversations subTargets makeDocX
  where
  makeDocX :: Mp.Map Text GfTarget -> Jd.Conversation -> IO ()
  makeDocX targetMap conversation =
    case Op.analyzeDiscussion conversation of
      Left err -> putStrLn $ "@[genDocx] error: " <> unpack err
      Right context ->
        let
          outPath = "/tmp/" <> unpack conversation.convIdCv <> ".docx"
        in do
        Gd.writeContextDocx context conversation.titleCv outPath
        putStrLn $ "@[genDocx] wrote to " <> outPath


storeDiscussions :: [Jd.Conversation] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
storeDiscussions conversations dbPool =
  let
    subTargets = gfTargets
  in do
  runOnDiscussionSubset conversations subTargets storeDiscussion
  pure . Right . Right $ []
  where
  storeDiscussion :: Mp.Map Text GfTarget -> Jd.Conversation -> IO ()
  storeDiscussion targetMap conversation = do
    putStrLn . unpack $ "Title: " <> conversation.titleCv <> ", id: " <> conversation.convIdCv
    let
      rezCtx = Op.analyzeDiscussion conversation
    case rezCtx of
      Left err -> putStrLn $ "@[storeDiscussions] error: " <> unpack err
      Right context -> do
        rez <- Sdc.storeDiscussion dbPool conversation.titleCv conversation.convIdCv context
        case rez of
          Left err -> putStrLn $ "@[storeDiscussions] error: " <> err
          Right _ -> putStrLn $ "@[storeDiscussions] stored discussion: " <> unpack conversation.titleCv <> ", id: " <> unpack conversation.convIdCv


{-
genDocxFromDiscs :: FilePath -> [ItemIdent] -> Hp.Pool -> IO (Either a1 (Either a2 [Either [String] [Ddc.DiscussionDb]]))
genDocxFromDiscs destPath targets dbPool = do
  rezA <- mapM (\target -> genDocxByConvId destPath target.uidGF dbPool) targets
  pure . Right . Right $ rights rezA
-}

genDocxByDiscId :: Hp.Pool -> FilePath -> ItemIdent -> IO (Either Hp.UsageError (Either String Int64))
genDocxByDiscId dbPool destPath itemID = do
  eiRez <- case itemID of
    EidCI eid -> Ddc.findDiscussionByConvId dbPool eid
    UidCI uid -> Ddc.findDiscussionByUid dbPool uid
  case eiRez of
    Left err -> do
      putStrLn $ "@[genDocxByConvId] err: " <> err
      pure . Right $ Left err
    Right mbUuid ->
      case mbUuid of
        Nothing -> do
          putStrLn $ "@[genDocxByConvId] no discourse found for id: " <> show itemID
          pure . Right . Left $ "@[genDocxByConvId] no discourse found for id: " <> show itemID
        Just uuid -> do
          genDocxDb destPath uuid dbPool


genDocxDb :: FilePath -> UUID -> Hp.Pool -> IO (Either Hp.UsageError (Either String Int64))
genDocxDb destPath discourseId dbPool = do
  eiRez <- Ddc.loadDiscussion dbPool discourseId
  case eiRez of
    Left err -> do
      -- putStrLn $ "@[genDocxDb] error: " <> err
      pure $ Left err
    Right eiMbDiscourse ->
      case eiMbDiscourse of
        Left err ->
          pure . Right $ Left err
        Right mbDiscourse ->
          case mbDiscourse of
            Nothing -> do
              -- putStrLn $ "@[genDocxDb] no discourse found for id: " <> show discourseId
              pure . Right . Left $ "@[genDocxDb] no discourse found for id: " <> show discourseId
            Just discourse ->
              let
                nameFromTitle = T.replace " " "_" discourse.titleCo
                outPath = destPath </> unpack nameFromTitle <> "_" <> show discourse.convIdCo <> ".docx"
              in do
              Gdb.writeDiscussionDbDocx discourse discourse.titleCo outPath
              putStrLn $ "@[genDocxDb] wrote to " <> outPath
              pure . Right $ Right discourse.refCo.uidRd


genSummaries :: Opt.TargetsOpts -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [()]))
genSummaries targetOpts dbPool =
  case targetOpts.groupTO of
    Just aName -> do
      eiDiscussions <- Ddc.allDiscussionsInGroup dbPool aName
      case eiDiscussions of
        Left err -> pure . Left $ [err]
        Right discussions ->
          let
            targetIDs = V.toList $ V.map (\(_, _, uuid) -> uuid) discussions
          in do
          httpManager <- Sm.newOllamaManager
          rezA <- mapM (genSummariesByDiscEid dbPool httpManager) targetIDs
          pure $ Utl.listResultsToResultList rezA
    Nothing -> case targetOpts.targetsTO of
      [] -> pure . Right $ Left ["@[genSummaries] no targets to summarize."]
      someTargets ->
        let
          targetIDs = map EidCI someTargets
        in do
        httpManager <- Sm.newOllamaManager
        -- TODO: recover the proper errors from the inner db ops.
        rezA <- mapM (\aTarget -> do
          putStrLn $ "@[genSummaries] searching: " <> show aTarget
          eiRez <- case aTarget of
            EidCI eid -> Ddc.findDiscussionByConvId dbPool eid
            UidCI uid -> Ddc.findDiscussionByUid dbPool uid
          case eiRez of
            Left err ->
              pure . Right $ Left err
            Right mbDiscourse ->
              case mbDiscourse of
                Nothing ->
                  pure . Right $ Left $ "no discourse found for id: " <> show aTarget
                Just eid -> genSummariesByDiscEid dbPool httpManager eid
          ) targetIDs
        pure $ Utl.listResultsToResultList rezA


genSummariesByDiscEid :: Hp.Pool -> Ht.Manager -> UUID -> IO (Either Hp.UsageError (Either String ()))
genSummariesByDiscEid dbPool httpManager eid = do
  eiRezB <- Ddc.loadDiscussion dbPool eid
  case eiRezB of
    Left err ->
      pure . Left $ err
    Right eiMbDiscourse ->
      case eiMbDiscourse of
        Left err ->
          pure . Right $ Left err
        Right mbDiscourse ->
          case mbDiscourse of
            Nothing ->
              pure . Right . Left $ "no discourse found for id: " <> show eid
            Just discourse -> do
              -- putStrLn $ "@[genSummaries] summarizing: " <> unpack discourse.titleCo
              rezA <- Sm.summarizeDiscourseMessages dbPool httpManager discourse
              case rezA of
                Left errs -> pure . Left $ head errs
                Right opResults -> case opResults of
                  Left errs -> pure . Right . Left $ L.intercalate "; " errs
                  Right _ -> pure . Right $ Right ()

-- Hp.Pool -> FilePath -> (Int, ItemIdent) -> IO (Either Hp.UsageError (Either String FilePath))

generateElmFromItem :: OperFunction FilePath
generateElmFromItem dbPool destPath (index, item) = do
  case item of
    EidCI eid -> do
      Em.elmifyDiscussionByConvEid destPath eid dbPool ("D" <> (T.pack . show) index)
    UidCI uid ->
      pure . Right . Left $ "UID not supported for Elm generation"
    UuidCI uuid ->
      Em.elmifyDiscussionByUuid destPath uuid dbPool ("D" <> (T.pack . show) index)


{-
generateElmFiles :: FilePath -> [GfTarget] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Text]))
generateElmFiles destPath targets dbPool = do
  rezA <- mapM (\(index, target) -> do
    putStrLn $ "@[generateElmFiles] saving: " <> unpack target.uidGF
    eiRez <- Em.elmifyDiscourseByEid destPath target.uidGF dbPool ("D" <> (T.pack . show) index)
    case eiRez of
      Left err -> do
        putStrLn $ "@[generateElmFiles] error: " <> show err
        pure . Left $ err
      Right elm ->
        pure . Right $ Right [ elm ]
    ) $ zip [1..] gfTargets
  pure . Right . Right $ rights []
-}

-- Deprecated:
saveConversationToDocx :: Opt.OaiGenOpts -> Rto.RunOptions -> IO ()
saveConversationToDocx genOpts rtOpts =
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in
  putStrLn $ "@[saveConversationToDocx] save to docx not implemented yet"
  {-
  Mc.runContT pgPool (conversationToDocx genOpts.destPath)
  reportDbErrors "saveConversationToDocx" rezA
  where
  conversationToDocx :: FilePath -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
  conversationToDocx destPath target pgPool = do
    case genOpts.targets of
      [] -> do
        putStrLn $ "@[saveConversationToDocx] no targets to save."
        pure . Right $ Left [ "no targets to save" ]
      targets -> do
        rezA <- mapM (\target -> do
          eiRez <- Ddc.findDiscussionByConvId dbPool target
          case eiRez of
            Left err -> do
              putStrLn $ "@[genDocxByConvId] err: " <> err
              pure . Right $ Left [ err ]
            Right mbUuid ->
              case mbUuid of
                Nothing -> do
                  putStrLn $ "@[genDocxByConvId] no discourse found for id: " <> show convId
                  pure . Right $ Left [ "no discourse found" ]
                Just uuid -> do
                  genDocxDb destPath uuid pgPool
          ) targets
        pure ()
  -}
{-
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in do
    rezA <- Mc.runContT pgPool (saveElmFiles gfTargets)
    -- rezA <- Mc.runContT pgPool (genSummaries gfTargets)
    -- rezA <- Mc.runContT pgPool (genAllDocxs gfTargets)
    pure ()
    {-
    rezA <- Mc.runContT pgPool (storeDiscussions discussions)   -- saveConversations ; storeDiscussions << discussions
    case rezA of
      Left err -> do
        putStrLn $ "@[parseCmd] saving discussions failed: " <> show err
      Right apiRez -> do
        case apiRez of
          Right successInResults ->
            putStrLn $ "@[parseCmd] saved " <> show (length successInResults) <> " discussions."
          Left errorInResults ->
            putStrLn $ "@[parseCmd] logic errors: " <> L.intercalate "\n" errorInResults
    -}

-}