{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Commands.OpenAI where

import qualified Control.Monad.Cont as Mc
import Control.Monad (unless)

import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import qualified Data.List as L
import Data.Either (lefts, rights)
import Data.Text (Text, unpack, intercalate)
import qualified Data.Text.IO as T
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.Map as Mp

import qualified Data.Aeson as Ae

import qualified Hasql.Pool as Hp

import qualified Options.Runtime as Rto
import qualified Options.Types as Opt
import qualified DB.Connect as Dbc
import qualified OpenAI.Json.Reader as Jd
import qualified OpenAI.Parse as Op
import qualified OpenAI.Operations as Dbo
import qualified OpenAI.InOperations as InO
import qualified OpenAI.GenDocx as Gd

parseCmd :: Opt.OaiOpts -> Rto.RunOptions -> IO ()
parseCmd opts rtOpts = do
  jsonContent <- BS.readFile opts.jsonFile
  let
    rezA = if opts.exportB then
      Ae.eitherDecode jsonContent :: Either String [Jd.Discussion]
    else
      L.singleton <$> (Ae.eitherDecode jsonContent :: Either String Jd.Discussion)
  case rezA of
    Left err -> putStrLn $ "Parsing failed: " ++ err
    Right discussions ->
      let
        pgPool = Dbc.startPg rtOpts.pgDbConf
      in do
      putStrLn $ "@[parseCmd] loaded " <> show (length discussions) <> " discussions."
      if opts.printB then do
        putStrLn "@[parseCmd] printing."
        -- showDiscussions discussions
        -- mapDiscussionsToElm discussions
        -- extractGFContent discussions
        genDocx discussions
      else do
        rezA <- Mc.runContT pgPool (loadDiscourse "1bda9104-a74f-450b-9e5b-48a5b3f85b2c")   -- saveDiscussions ; storeDiscussions << discussions
        case rezA of
          Left err -> do
            putStrLn $ "@[parseCmd] saving discussions failed: " <> show err
          Right apiRez -> do
            case apiRez of
              Right successInResults ->
                putStrLn $ "@[parseCmd] saved " <> show (length successInResults) <> " discussions."
              Left errorInResults ->
                putStrLn $ "@[parseCmd] logic errors: " <> L.intercalate "\n" errorInResults 
    


saveDiscussions :: [Jd.Discussion] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
saveDiscussions discussions pgPool = do
  discussionMap <- Dbo.fetchAllDiscussions pgPool
  case discussionMap of
    Left err -> pure . Left $ [err]
    Right discussionMap ->
      let
        newDiscussions = filter (\discussion -> not (Mp.member discussion.titleCv discussionMap)) discussions
      in
      case newDiscussions of
        [] -> do
          putStrLn $ "@[saveDiscussions] no new discussions to save."
          pure . Right . Right $ []
        _ -> do
          putStrLn $ "@[saveDiscussions] saving " <> show (length newDiscussions) <> " new discussions."
          results <- mapM (Dbo.addDiscussion pgPool) newDiscussions
          if null (lefts results) then
            let
              errorInResults = lefts $ rights results
              successInResults = rights $ rights results
            in
            if null errorInResults then
              pure . Right . Right $ successInResults
            else
              pure . Right . Left $ errorInResults
          else
            pure (Left (lefts results))


showDiscussions :: [Jd.Discussion] -> IO ()
showDiscussions = do
  mapM_ (\discussion -> do
      -- putStrLn "Parsing successful. Loaded Discussion:"
      -- print discussion
      -- Example of using OverloadedRecordDot: access title
      putStrLn . unpack $ "Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv
      let
        (analysis, issues) = Op.showDiscussion discussion
      putStrLn . unpack $ analysis
      unless (null issues) $
        putStrLn $ "@[showDiscussions] issues: " <> L.intercalate "\n" (map unpack issues)
    )


mapDiscussionsToElm :: [Jd.Discussion] -> IO ()
mapDiscussionsToElm discussions = do
  mapM_ (\discussion -> do
    putStrLn . unpack $ "Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv
    let
      rezElm = Op.discussionToElm discussion
    case rezElm of
      Left err -> putStrLn $ "@[mapDiscussionsToElm] error: " <> unpack err
      Right elm -> putStrLn . unpack $ elm
    ) discussions


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
  


runOnDiscussionSubset :: [Jd.Discussion] -> [GfTarget] -> (Mp.Map Text GfTarget -> Jd.Discussion -> IO ()) -> IO ()
runOnDiscussionSubset discussions targets evalFct =
  let
    targetMap = Mp.fromList $ map (\target -> (target.uidGF, target)) targets
    targetDiscussions = filter (\discussion -> Mp.member discussion.convIdCv targetMap) discussions
  in
  mapM_ (evalFct targetMap) targetDiscussions


extractGFContent :: [Jd.Discussion] -> IO ()
extractGFContent discussions =
  runOnDiscussionSubset discussions gfTargets makeElmFile
  where
  makeElmFile :: Mp.Map Text GfTarget -> Jd.Discussion -> IO ()
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
            T.writeFile outPath elmContent
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
  
genDocx :: [Jd.Discussion] -> IO ()
genDocx discussions =
  let
    subTargets = take 1 gfTargets
  in
  runOnDiscussionSubset discussions subTargets makeDocX
  where
  makeDocX :: Mp.Map Text GfTarget -> Jd.Discussion -> IO ()
  makeDocX targetMap discussion =
    case Op.analyzeDiscussion discussion of
      Left err -> putStrLn $ "@[genDocx] error: " <> unpack err
      Right context ->
        let
          outPath = "/tmp/" <> unpack discussion.convIdCv <> ".docx"
        in do
        Gd.writeContextDocx context discussion.titleCv outPath
        putStrLn $ "@[genDocx] wrote to " <> outPath


storeDiscussions :: [Jd.Discussion] -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [Int64]))
storeDiscussions discussions dbPool =
  let
    subTargets = take 1 gfTargets
  in do
  runOnDiscussionSubset discussions subTargets storeDiscussion
  pure . Right . Right $ []
  where
  storeDiscussion :: Mp.Map Text GfTarget -> Jd.Discussion -> IO ()
  storeDiscussion targetMap discussion = do
    putStrLn . unpack $ "Title: " <> discussion.titleCv <> ", id: " <> discussion.convIdCv
    let
      rezCtx = Op.analyzeDiscussion discussion
    case rezCtx of
      Left err -> putStrLn $ "@[storeDiscussions] error: " <> unpack err
      Right context -> do
        rez <- Dbo.storeDiscussion dbPool context
        case rez of
          Left err -> putStrLn $ "@[storeDiscussions] error: " <> err
          Right _ -> putStrLn $ "@[storeDiscussions] stored discussion: " <> unpack discussion.titleCv <> ", id: " <> unpack discussion.convIdCv
  

loadDiscourse :: String -> Hp.Pool -> IO (Either [Hp.UsageError] (Either [String] [InO.ContextDb]))
loadDiscourse discourseId dbPool =
  case Uu.fromString discourseId of
    Nothing -> do
      putStrLn $ "@[loadDiscourse] invalid discourse id: " <> show discourseId
      pure . Right $ Left [ "invalid discourse id" ]
    Just aUuid -> do
      eiRez <- InO.loadDiscourse dbPool aUuid
      case eiRez of
        Left err -> do
          putStrLn $ "@[loadDiscourse] error: " <> err
          pure . Right $ Left [ err ]
        Right mbDiscourse -> 
          case mbDiscourse of
            Nothing -> do
              putStrLn $ "@[loadDiscourse] no discourse found for id: " <> show discourseId
              pure . Right $ Left [ "no discourse found" ]
            Just discourse -> do
              putStrLn $ "@[loadDiscourse] discourse: " <> show discourse
              pure . Right $ Right [discourse]