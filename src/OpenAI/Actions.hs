module OpenAI.Actions where

import qualified Control.Monad.Cont as Mc
import Control.Monad (unless)

import qualified Data.ByteString.Lazy as Bsl
import Data.Either (lefts, rights, partitionEithers)
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Mp
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu
import qualified Data.Vector as V

import qualified Data.Aeson as Ae

import System.FilePath ((</>))

import qualified Hasql.Pool as Hp
import qualified Network.HTTP.Client as Ht

import qualified DB.Connect as Dbc
import qualified Options.Runtime as Rto
import qualified Options.Types as Opt

import qualified OpenAI.Conversation as Cv
import qualified OpenAI.ConvToDisc as Ccv
import qualified OpenAI.Generate.Docx as Gd
import qualified OpenAI.Generate.DocxDb as Gdb
import qualified OpenAI.Generate.Elmify as Em
import qualified OpenAI.Import.Batch as Ib
import qualified OpenAI.Import.Report as Ir
import qualified OpenAI.Import.Types as Im
import qualified OpenAI.Conversation.Json.Reader as Jr
import qualified OpenAI.Conversation.Json.Schema as Js
import qualified OpenAI.Conversation.Process as Op
import qualified OpenAI.ProjFetcher as Pf
import qualified OpenAI.Conversation.Deserialize.Conversation as Dcv
import qualified OpenAI.Discussion.Serialize.Discussion as Sdc
import qualified OpenAI.Discussion.Deserialize.Discussion as Ddc
import qualified OpenAI.Summarisation as Sm
import qualified OpenAI.Utils as Utl


data ItemIdent
  = EidCI Text
  | UidCI Int64
  | UuidCI UUID
  deriving (Show)


data GfTarget = GfTarget
  { fileIdGF :: Text
  , titleGF :: Text
  , uidGF :: Text
  } deriving (Show)


gfTargets :: [GfTarget]
gfTargets =
  let rawEntries = []
  in map (\(fileId, title, uid) -> GfTarget fileId title uid) rawEntries


parseJson :: FilePath -> Bool -> IO (Either String [(Js.Conversation, Text)])
parseJson jsonFile exportB = do
  jsonContent <- Bsl.readFile jsonFile
  if exportB then
    let
      eiConversations = Ae.eitherDecode jsonContent :: Either String [Js.Conversation]
    in
    case eiConversations of
      Left errMsg -> pure $ Left errMsg
      Right conversations -> do
        convKeys <- mapM (const Uu.nextRandom) conversations
        pure . Right $ zipWith (\conv key -> (conv, Uu.toText key)) conversations convKeys
  else
    case Jr.parse jsonContent of
      Left errMsg -> pure $ Left errMsg
      Right conv -> do
        -- putStrLn $ "@[parseJson] conversation: " <> T.unpack conv.oaiIdCv
        pure . Right $ [(conv, conv.oaiIdCv)]


parseBrowserJson :: [FilePath] -> IO (Either String [(Js.Conversation, Text)])
parseBrowserJson jsonFiles =
  case jsonFiles of
    [] -> pure $ Left "@[parseBrowserJson] no json files provided"
    [jsonFile] -> do
      jsonContent <- Bsl.readFile jsonFile
      case Jr.parse jsonContent of
        Left errMsg -> pure $ Left errMsg
        Right conv -> pure $ Right [(conv, conv.oaiIdCv)]
    jsonFile : followFiles -> do
      rezA <- parseJson jsonFile False
      case rezA of
        Left err -> pure $ Left err
        Right [(conv, oaiID)] -> do
          followRez <- mapM parseFollowJson followFiles
          case partitionEithers followRez of
            ([], followConvs) -> do
              case Jr.mergeBrowseJson conv followConvs of
                Left err -> pure $ Left err
                Right mergedConv -> pure $ Right [(mergedConv, oaiID)]
            (errs, _) -> pure $ Left $ "@[parseBrowserJson] " <> show errs
        _ -> pure $ Left "@[parseBrowserJson] unexpected conversation format"


parseFollowJson :: FilePath -> IO (Either String Js.FollowConv)
parseFollowJson jsonFile = do
  jsonContent <- Bsl.readFile jsonFile
  case Jr.parseFollow jsonContent of
    Left errMsg -> pure $ Left errMsg
    Right followConv -> pure $ Right followConv


printJson :: Opt.OaiPrintOpts -> Opt.TargetsOpts -> IO ()
printJson printOpts targetsOpts = do
  rezA <- parseJson printOpts.jsonFilePR printOpts.exportPrB
  case rezA of
    Left err -> putStrLn $ "Parsing failed: " <> err
    Right conversations -> do
      let
        (targetConvs, missingTargets) = selectJsonTargets conversations targetsOpts
      reportMissingTargets "printJson" missingTargets
      if null targetConvs then
        putStrLn "@[printJson] no conversations selected."
      else
        mapM_ showConversation targetConvs


storeJsonAsConversations :: Opt.OaiStoreOpts -> Opt.TargetsOpts -> Rto.RunOptions -> IO ()
storeJsonAsConversations storeOpts targetsOpts rtOpts = case storeOpts.followFilesST of
  [] -> do
    rezA <- parseJson storeOpts.jsonFileST storeOpts.exportB
    case rezA of
      Left err -> putStrLn $ "Parsing failed: " <> err
      Right conversations -> do
        printConvInfo conversations
        let
          (targetConvs, missingTargets) = selectJsonTargets conversations targetsOpts
        reportMissingTargets "storeJsonAsConversations" missingTargets
        if null targetConvs then
          putStrLn "@[storeJsonAsConversations] no conversations selected."
        else
          let
            source = sourceFromStore storeOpts
            opts = optsFromStore storeOpts
            pgPool = Dbc.startPg rtOpts.pgDbConf
          in do
          rezB <- Mc.runContT pgPool $ \pool -> Ib.runBatch Nothing pool source opts targetConvs
          case rezB of
            Left errs -> putStrLn $ "@[storeJsonAsConversations] db err: " <> show errs
            Right batch -> Tio.putStrLn $ Ir.renderBatch batch
  followFiles -> do
    rezA <- parseBrowserJson $ storeOpts.jsonFileST : followFiles
    case rezA of
      Left err -> putStrLn $ "Parsing failed: " <> err
      Right conversations -> do
        printConvInfo conversations
        let
          (targetConvs, missingTargets) = selectJsonTargets conversations targetsOpts
        reportMissingTargets "storeJsonAsConversations" missingTargets


printConvInfo :: [(Js.Conversation, Text)] -> IO ()
printConvInfo conversations = do
  mapM_ (\(conv, oaiID) -> putStrLn $ "@[printConvInfo] conversation: " <> T.unpack oaiID <> ", msgs: " <> show (length conv.messagesCv)) conversations

sourceFromStore :: Opt.OaiStoreOpts -> Im.Source
sourceFromStore storeOpts =
  Im.Source
    { pathSrc = Just storeOpts.jsonFileST
    , exportSrc = storeOpts.exportB
    , labelSrc = Nothing
    }


optsFromStore :: Opt.OaiStoreOpts -> Im.Opts
optsFromStore storeOpts =
  Im.Opts
    { modeOpt =
        if storeOpts.dryRunB then Im.DryM else Im.WriteM
    , scopeOpt = Im.RawOnlyS
    , policyOpt =
        Im.Policy
          { allowOlderPol = False
          , allowRepairPol = False
          , stopOnFailPol = False
          }
    }


reportDbErrors :: String -> Either [Hp.UsageError] (Either [String] [resultT]) -> IO ()
reportDbErrors opLabel eiRez =
  case eiRez of
    Left errs ->
      putStrLn $ "@[" <> opLabel <> "] db err: " <> show errs
    Right (Left errs) ->
      putStrLn $ "@[" <> opLabel <> "] logic err: " <> show errs
    Right (Right results) ->
      putStrLn $ "@[" <> opLabel <> "] saved " <> show (length results) <> "."


saveSummaries :: Opt.TargetsOpts -> Rto.RunOptions -> IO ()
saveSummaries summaryOpts rtOpts = do
  let pgPool = Dbc.startPg rtOpts.pgDbConf
  rezA <- Mc.runContT pgPool (genSummaries summaryOpts)
  reportDbErrors "saveSummaries" rezA


type OperFunction resultT =
  Hp.Pool -> FilePath -> (Int, ItemIdent) -> IO (Either Hp.UsageError (Either String resultT))


doOnTargets :: Opt.OaiGenOpts -> OperFunction resultT -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [resultT]))
doOnTargets genOpts operation pgPool =
  case genOpts.group of
    Nothing ->
      case genOpts.targets of
        [] ->
          pure . Right $ Left ["@[doOnTargets] no targets to save."]
        someTargets -> do
          let targets = map EidCI someTargets
          putStrLn $
            "@[doOnTargets] targets: "
            <> show someTargets
            <> ", saving "
            <> show (length targets)
            <> " targets."
          rezA <- mapM (operation pgPool genOpts.destPath) (zip [1 ..] targets)
          pure $ Utl.listResultsToResultList rezA
    Just groupName -> do
      eiDiscussions <- Ddc.allDiscussionsInGroup pgPool groupName
      case eiDiscussions of
        Left err ->
          pure . Left $ [err]
        Right discussions -> do
          let targetIds = V.toList $ V.map (\(_, _, uuid) -> UuidCI uuid) discussions
          putStrLn $
            "@[doOnTargets] group: "
            <> T.unpack groupName
            <> ", saving "
            <> show (length targetIds)
            <> " targets."
          rezA <- mapM (operation pgPool genOpts.destPath) (zip [1 ..] targetIds)
          pure $ Utl.listResultsToResultList rezA


saveDocx :: Opt.OaiGenOpts -> Rto.RunOptions -> IO ()
saveDocx genOpts rtOpts = do
  let pgPool = Dbc.startPg rtOpts.pgDbConf
  rezA <- Mc.runContT pgPool (saveDocxFromDiscs genOpts)
  reportDbErrors "saveDocx" rezA


saveProject :: Opt.OaiProjFetchOpts -> Rto.RunOptions -> IO ()
saveProject fetchOpts rtOpts = do
  projects <- Pf.saveProjects fetchOpts.sourcePath
  putStrLn $
    "Group: "
    <> T.unpack fetchOpts.label
    <> ", Projects ("
    <> show (length projects)
    <> ")"
  let pgPool = Dbc.startPg rtOpts.pgDbConf
  rezA <- Mc.runContT pgPool (Pf.saveDescriptionsToGroup fetchOpts.label projects)
  reportDbErrors "saveProject" rezA


deserializeConversation :: Opt.OaiGenOpts -> Rto.RunOptions -> IO ()
deserializeConversation genOpts rtOpts =
  let pgPool = Dbc.startPg rtOpts.pgDbConf
  in case genOpts.group of
    Just groupName ->
      putStrLn $
        "@[deserializeConversation] deserializing conversation: "
        <> genOpts.destPath
        <> " for group: "
        <> T.unpack groupName
    Nothing ->
      case genOpts.targets of
        [] ->
          putStrLn "@[deserializeConversation] no targets to deserialize."
        targets -> do
          rezA <- Mc.runContT pgPool (convDeserialize targets genOpts.destPath)
          reportDbErrors "deserializeConversation" rezA


convDeserialize :: [Text] -> FilePath -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Cv.ConversationDb]))
convDeserialize targets destPath pgPool = do
  results <- mapM onTarget targets
  case lefts results of
    [] ->
      case lefts (rights results) of
        [] -> pure . Right . Right $ rights (rights results)
        errMsgs -> pure . Right $ Left errMsgs
    errs -> pure $ Left errs
  where
    onTarget :: Text -> IO (Either Hp.UsageError (Either String Cv.ConversationDb))
    onTarget target = do
      eiErrRez <- Dcv.getConversationByEid pgPool target
      case eiErrRez of
        Left err ->
          pure $ Left err
        Right eiMbConv ->
          case eiMbConv of
            Left errMsg ->
              pure . Right $ Left errMsg
            Right mbConv ->
              case mbConv of
                Nothing ->
                  pure . Right $ Left "no conversation found"
                Just convDb -> do
                  putStrLn $
                    "@[convertConversation] deserializing conversation: "
                    <> destPath
                    <> " for target: "
                    <> T.unpack target
                  case Ccv.analyzeConversation convDb of
                    Left errMsgA ->
                      pure . Right . Left $ T.unpack errMsgA
                    Right context -> do
                      rezA <- Gd.writeContextDocx context convDb.titleCv (destPath </> T.unpack convDb.eidCv <> ".docx")
                      case rezA of
                        Left errMsgB -> do
                          putStrLn $ "@[convertConversation] error: " <> errMsgB
                          pure . Right $ Left errMsgB
                        Right _ ->
                          pure . Right $ Right convDb


convertConversation :: Opt.TargetsOpts -> Rto.RunOptions -> IO ()
convertConversation targetOpts rtOpts =
  let pgPool = Dbc.startPg rtOpts.pgDbConf
  in case targetOpts.groupTO of
    Just groupName ->
      putStrLn $ "@[convertConversation] group not supported yet: " <> T.unpack groupName
    Nothing ->
      case targetOpts.targetsTO of
        [] -> do
          rezA <- Mc.runContT pgPool convStoreAllConversations
          reportDbErrors "convStoreAllConversations" rezA
        targets -> do
          let targetIds = map EidCI targets
          rezA <- Mc.runContT pgPool (convStoreDiscussions targetIds)
          reportDbErrors "convStoreDiscussion" rezA


convStoreAllConversations :: Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
convStoreAllConversations pgPool = do
  eiConversations <- Dcv.fetchAllConversations pgPool
  case eiConversations of
    Left err ->
      pure . Left $ [err]
    Right conversations ->
      let targetIds = map UidCI (Mp.elems conversations)
      in convStoreDiscussions targetIds pgPool


convStoreDiscussions :: [ItemIdent] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
convStoreDiscussions targets pgPool = do
  results <- mapM (convStoreADiscussion pgPool) targets
  case lefts results of
    [] ->
      case lefts (rights results) of
        [] -> pure . Right . Right $ rights (rights results)
        errMsgs -> pure . Right $ Left errMsgs
    errs -> pure $ Left errs


convStoreADiscussion :: Hp.Pool -> ItemIdent -> IO (Either Hp.UsageError (Either String Int64))
convStoreADiscussion pgPool target = do
  eiErrRez <- case target of
    EidCI eid -> Dcv.getConversationByEid pgPool eid
    UidCI uid -> Dcv.getConversationByUid pgPool uid
    UuidCI uuid -> pure . Right . Left $ "UUID target not supported for conversation lookup: " <> show uuid
  case eiErrRez of
    Left err ->
      pure $ Left err
    Right eiMbConv ->
      case eiMbConv of
        Left errMsg ->
          pure . Right $ Left errMsg
        Right mbConv ->
          case mbConv of
            Nothing ->
              pure . Right $ Left "no conversation found"
            Just convDb -> do
              putStrLn $ "@[convStoreDiscussion] deserializing for target: " <> show target
              case Ccv.analyzeConversation convDb of
                Left errMsgA ->
                  pure . Right . Left $ T.unpack errMsgA
                Right context -> do
                  rezA <- Sdc.storeDiscussion pgPool convDb.titleCv convDb.eidCv context
                  case rezA of
                    Left errMsgB -> do
                      putStrLn $ "@[convertConversation] error: " <> errMsgB
                      pure . Right $ Left errMsgB
                    Right (ctxUid, _) ->
                      pure . Right $ Right ctxUid


saveConversations :: [(Js.Conversation, Text)] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
saveConversations conversations pgPool = do
  putStrLn "@[saveConversations] deprecated; use OpenAI.Import.Batch.runBatch."
  let source =
        Im.Source
          { pathSrc = Nothing
          , exportSrc = False
          , labelSrc = Just "saveConversations"
          }
  let opts =
        Im.Opts
          { modeOpt = Im.WriteM
          , scopeOpt = Im.RawOnlyS
          , policyOpt =
              Im.Policy
                { allowOlderPol = False
                , allowRepairPol = False
                , stopOnFailPol = False
                }
          }
  rezA <- Ib.runBatch Nothing pgPool source opts conversations
  pure $
    case rezA of
      Left errs ->
        Left errs
      Right batch ->
        let failMsgs = map (\eid -> "@[saveConversations] failed: " <> T.unpack eid) batch.failedEids
            convUids = mapMaybe (\report -> report.uidConv) batch.reports
        in if null failMsgs then
             Right $ Right convUids
           else
             Right $ Left failMsgs


showConversation :: (Js.Conversation, Text) -> IO ()
showConversation (conversation, sourceKey) = do
  putStrLn . T.unpack $ "Title: " <> conversation.titleCv <> ", id: " <> conversation.oaiIdCv
  let (analysis, issues) = Op.toText conversation
  putStrLn . T.unpack $ analysis
  unless (null issues) $
    putStrLn $ "@[showConversation] issues: " <> L.intercalate "\n" (map T.unpack issues)


mapDiscussionsToElm :: [Js.Conversation] -> IO ()
mapDiscussionsToElm =
  mapM_
    (\conversation -> do
      putStrLn . T.unpack $ "Title: " <> conversation.titleCv <> ", id: " <> conversation.oaiIdCv
      case Op.toElm conversation of
        Left err ->
          putStrLn $ "@[mapDiscussionsToElm] error: " <> T.unpack err
        Right elm ->
          putStrLn . T.unpack $ elm
    )


runOnDiscussionSubset :: [Js.Conversation] -> [GfTarget] -> (Mp.Map Text GfTarget -> Js.Conversation -> IO ()) -> IO ()
runOnDiscussionSubset conversations targets evalFct =
  let targetMap = Mp.fromList $ map (\target -> (target.uidGF, target)) targets
      targetConvs = filter (\conversation -> Mp.member conversation.oaiIdCv targetMap) conversations
  in mapM_ (evalFct targetMap) targetConvs


extractGFContent :: [Js.Conversation] -> IO ()
extractGFContent conversations =
  runOnDiscussionSubset conversations gfTargets makeElmFile
  where
    makeElmFile :: Mp.Map Text GfTarget -> Js.Conversation -> IO ()
    makeElmFile targetMap conversation = do
      putStrLn . T.unpack $ "Title: " <> conversation.titleCv <> ", id: " <> conversation.oaiIdCv
      case Op.toElm conversation of
        Left err ->
          putStrLn $ "@[extractGFContent] error: " <> T.unpack err
        Right elm ->
          case Mp.lookup conversation.oaiIdCv targetMap of
            Just target -> do
              let outPath = "/tmp/" <> T.unpack target.fileIdGF <> ".elm"
              let elmContent = elmPreambule target <> "\nmessages = " <> elm <> "\n"
              Tio.writeFile outPath elmContent
              putStrLn $ "@[extractGFContent] wrote to " <> outPath
            Nothing -> putStrLn $ "@[extractGFContent] no target found for discussion: " <> T.unpack conversation.titleCv
                <> ", id: " <> T.unpack conversation.oaiIdCv

    elmPreambule :: GfTarget -> Text
    elmPreambule target =
      let
        moduleDef = "module LegalNodes.Docs." <> target.fileIdGF <> " exposing (content)\n"
        importDef = "import Components.LegalNodes.ReferenceDoc.Types as T\n"
        contentDef = "content = (\"" <> target.titleGF <> "\", \"" <> target.uidGF <> "\", messages)"
      in T.intercalate "\n" [moduleDef, importDef, contentDef]


saveDocxFromDiscs :: Opt.OaiGenOpts -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
saveDocxFromDiscs genOpts pgPool =
  case genOpts.group of
    Nothing ->
      case genOpts.targets of
        [] ->
          pure . Right $ Left ["@[saveDocxFromDiscs] no targets to save."]
        someTargets -> do
          let targets = map EidCI someTargets
          rezA <- mapM (genDocxByDiscId pgPool genOpts.destPath) targets
          pure $ Utl.listResultsToResultList rezA
    Just groupName -> do
      eiDiscussions <- Ddc.allDiscussionsInGroup pgPool groupName
      case eiDiscussions of
        Left err ->
          pure . Left $ [err]
        Right discussions -> do
          let targetIds = V.toList $ V.map (\(uid, _, _) -> UidCI uid) discussions
          rezA <- mapM (genDocxByDiscId pgPool genOpts.destPath) targetIds
          pure $ Utl.listResultsToResultList rezA


genDocxFromConvs :: [Js.Conversation] -> IO ()
genDocxFromConvs conversations =
  let
    subTargets = take 1 gfTargets
  in
  runOnDiscussionSubset conversations subTargets makeDocX
  where
  makeDocX :: Mp.Map Text GfTarget -> Js.Conversation -> IO ()
  makeDocX _ conversation =
    let
      context = Op.analyze conversation
    in do
    let outPath = "/tmp/" <> T.unpack conversation.oaiIdCv <> ".docx"
    _ <- Gd.writeContextDocx context conversation.titleCv outPath
    putStrLn $ "@[genDocx] wrote to " <> outPath


storeDiscussions :: [Js.Conversation] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
storeDiscussions conversations dbPool = do
  runOnDiscussionSubset conversations gfTargets storeDiscussion
  pure . Right . Right $ []
  where
  storeDiscussion :: Mp.Map Text GfTarget -> Js.Conversation -> IO ()
  storeDiscussion _ conversation = do
    putStrLn . T.unpack $ "Title: " <> conversation.titleCv <> ", id: " <> conversation.oaiIdCv
    let
      context = Op.analyze conversation
    rez <- Sdc.storeDiscussion dbPool conversation.titleCv conversation.oaiIdCv context
    case rez of
      Left err -> putStrLn $ "@[storeDiscussions] error: " <> err
      Right _ -> putStrLn $ "@[storeDiscussions] stored discussion: " <> T.unpack conversation.titleCv
          <> ", id: " <> T.unpack conversation.oaiIdCv


genDocxByDiscId :: Hp.Pool -> FilePath -> ItemIdent -> IO (Either Hp.UsageError (Either String Int64))
genDocxByDiscId dbPool destPath itemId = do
  eiRez <- case itemId of
    EidCI eid -> Ddc.findDiscussionByConvId dbPool eid
    UidCI uid -> Ddc.findDiscussionByUid dbPool uid
    UuidCI uuid -> pure . Right $ Just uuid
  case eiRez of
    Left err -> do
      putStrLn $ "@[genDocxByConvId] err: " <> err
      pure . Right $ Left err
    Right mbUuid ->
      case mbUuid of
        Nothing -> do
          let errMsg = "@[genDocxByConvId] no discourse found for id: " <> show itemId
          putStrLn errMsg
          pure . Right $ Left errMsg
        Just uuid ->
          genDocxDb destPath uuid dbPool


genDocxDb :: FilePath -> UUID -> Hp.Pool -> IO (Either Hp.UsageError (Either String Int64))
genDocxDb destPath discourseId dbPool = do
  eiRez <- Ddc.loadDiscussion dbPool discourseId
  case eiRez of
    Left err ->
      pure $ Left err
    Right eiMbDiscourse ->
      case eiMbDiscourse of
        Left err ->
          pure . Right $ Left err
        Right mbDiscourse ->
          case mbDiscourse of
            Nothing ->
              pure . Right . Left $ "@[genDocxDb] no discourse found for id: " <> show discourseId
            Just discourse -> do
              let nameFromTitle = T.replace " " "_" discourse.titleCo
              let outPath = destPath </> T.unpack nameFromTitle <> "_" <> show discourse.convIdCo <> ".docx"
              _ <- Gdb.writeDiscussionDbDocx discourse discourse.titleCo outPath
              putStrLn $ "@[genDocxDb] wrote to " <> outPath
              pure . Right $ Right discourse.refCo.uidRd


genSummaries :: Opt.TargetsOpts -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [()]))
genSummaries targetOpts dbPool =
  case targetOpts.groupTO of
    Just groupName -> do
      eiDiscussions <- Ddc.allDiscussionsInGroup dbPool groupName
      case eiDiscussions of
        Left err ->
          pure . Left $ [err]
        Right discussions -> do
          let targetIds = V.toList $ V.map (\(_, _, uuid) -> uuid) discussions
          httpManager <- Sm.newOllamaManager
          rezA <- mapM (genSummariesByDiscEid dbPool httpManager) targetIds
          pure $ Utl.listResultsToResultList rezA
    Nothing ->
      case targetOpts.targetsTO of
        [] ->
          pure . Right $ Left ["@[genSummaries] no targets to summarize."]
        someTargets -> do
          let targetIds = map EidCI someTargets
          httpManager <- Sm.newOllamaManager
          rezA <- mapM (genOne httpManager) targetIds
          pure $ Utl.listResultsToResultList rezA
  where
    genOne :: Ht.Manager -> ItemIdent -> IO (Either Hp.UsageError (Either String ()))
    genOne httpManager target = do
      putStrLn $ "@[genSummaries] searching: " <> show target
      eiRez <- case target of
        EidCI eid -> Ddc.findDiscussionByConvId dbPool eid
        UidCI uid -> Ddc.findDiscussionByUid dbPool uid
        UuidCI uuid -> pure . Right $ Just uuid
      case eiRez of
        Left err ->
          pure . Right $ Left err
        Right mbDiscourse ->
          case mbDiscourse of
            Nothing ->
              pure . Right . Left $ "no discourse found for id: " <> show target
            Just discourseId ->
              genSummariesByDiscEid dbPool httpManager discourseId


genSummariesByDiscEid :: Hp.Pool -> Ht.Manager -> UUID -> IO (Either Hp.UsageError (Either String ()))
genSummariesByDiscEid dbPool httpManager discourseId = do
  eiRez <- Ddc.loadDiscussion dbPool discourseId
  case eiRez of
    Left err ->
      pure $ Left err
    Right eiMbDiscourse ->
      case eiMbDiscourse of
        Left err ->
          pure . Right $ Left err
        Right mbDiscourse ->
          case mbDiscourse of
            Nothing ->
              pure . Right . Left $ "no discourse found for id: " <> show discourseId
            Just discourse -> do
              rezA <- Sm.summarizeDiscourseMessages dbPool httpManager discourse
              case rezA of
                Left errs ->
                  pure . Left $ head errs
                Right opResults ->
                  case opResults of
                    Left errs ->
                      pure . Right . Left $ L.intercalate "; " errs
                    Right _ ->
                      pure . Right $ Right ()


generateElmFromItem :: OperFunction FilePath
generateElmFromItem dbPool destPath (index, item) =
  case item of
    EidCI eid ->
      Em.elmifyDiscussionByConvEid destPath eid dbPool ("D" <> (T.pack . show) index)
    UidCI _ ->
      pure . Right $ Left "UID not supported for Elm generation"
    UuidCI uuid ->
      Em.elmifyDiscussionByUuid destPath uuid dbPool ("D" <> (T.pack . show) index)


saveConversationToDocx :: Opt.OaiGenOpts -> Rto.RunOptions -> IO ()
saveConversationToDocx _ _ =
  putStrLn "@[saveConversationToDocx] save to docx not implemented yet"


selectJsonTargets :: [(Js.Conversation, Text)] -> Opt.TargetsOpts -> ([(Js.Conversation, Text)], [Text])
selectJsonTargets conversations targetsOpts =
  case targetsOpts.targetsTO of
    [] -> (conversations, [])
    targets ->
      let
        convByEid = Mp.fromList $ map (\(conversation, sourceKey) -> (conversation.oaiIdCv, (conversation, sourceKey))) conversations
        selected = mapMaybe (`Mp.lookup` convByEid) targets
        missing = filter (`Mp.notMember` convByEid) targets
      in (selected, missing)


reportMissingTargets :: String -> [Text] -> IO ()
reportMissingTargets opLabel missingTargets =
  unless (null missingTargets) $
    putStrLn $
      "@[" <> opLabel <> "] missing targets: "
      <> T.unpack (T.intercalate ", " missingTargets)


pluralS :: Int -> String
pluralS count =
  if count == 1 then "" else "s"