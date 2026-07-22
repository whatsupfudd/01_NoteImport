module Commands.Kms where

import Control.Monad.Cont (ContT (..))
import qualified Control.Monad.Cont as Mc
import qualified Control.Exception as Cex

import Data.Int (Int32)
import Data.Either (lefts, rights)
import qualified Data.List as L
import Data.Maybe (isNothing, fromJust, catMaybes)
import Data.Text (Text)
import qualified Data.UUID as Uu
import qualified Data.Vector as V

import Hasql.Pool (Pool, use, UsageError)
import qualified Hasql.Session as Hs

import qualified HBDoc.Manage.Types as HbT
import qualified HBDoc.Serialize.Statements as Ss
import qualified HBDoc.Serialize.Write as Sw
import qualified HBDoc.Manage.Operations as So

import qualified DB.Connect as Dbc
import Options.Cli (KmsSubCmd(..))
import qualified Options.Types as Ot
import qualified Options.Runtime as Rt


data CmdResult =
  SuccessCR
  | ErrorCR String
  | InfoCR Int32
  deriving (Show)


kmsCmd :: KmsSubCmd -> Rt.RunOptions -> IO ()
kmsCmd cmd rtOpts =
  let
    pgPool = Dbc.startPg rtOpts.pgDbConf
  in do
  rezA <- Mc.runContT pgPool (mainAction cmd)
  case rezA of
    Left err -> putStrLn $ "@[kmsCmd] command failed: " <> show err
    Right _ -> pure ()
  where
  mainAction :: KmsSubCmd -> Pool -> IO (Either String CmdResult)
  mainAction cmd dbPool =
    case cmd of
      ListKC opts -> listDocs opts dbPool
      CreateKC opts -> do
        rezA <- createKmsHbDoc opts dbPool
        case rezA of
          Left err -> pure . Left $ err
          Right cmdRez -> case cmdRez of
            InfoCR docID -> do
              putStrLn $ "@[kmsCmd] created doc: " <> show docID
              pure $ Right SuccessCR
            _ -> pure . Left $ "@[kmsCmd] createKmsHbDoc: unexpected cmd result: " <> show cmdRez
      DeleteKC key -> deleteKmsHbDoc key dbPool
      GetKC opts -> getKmsHbDoc opts dbPool


listDocs :: Ot.KmsLocatorOpts -> Pool -> IO (Either String CmdResult)
listDocs opts dbPool =
  if isNothing opts.title && isNothing opts.key then
    listAllDocs dbPool
  else
    case opts.key of
      Just key -> listDocsByKey key dbPool
      Nothing -> listDocsByTitle (fromJust opts.title) dbPool


createKmsHbDoc :: Ot.KmsCreateOpts -> Pool -> IO (Either String CmdResult)
createKmsHbDoc opts dbPool = do
  rezA <- So.resolveUser dbPool opts.emailDC
  rezF <- So.resolveUser dbPool opts.ownerUserDC
  case derefUsers rezA rezF of
    Left err -> pure . Left $ err
    Right (userID, ownerID) -> do
      rezB <- use dbPool $ Hs.statement opts.domainDC Ss.getDomainByName
      rezC <- use dbPool $ Hs.statement opts.typeDC Ss.getTypeByName
      rezD <- use dbPool $ Hs.statement opts.tierDC Ss.getTierByName
      rezE <- use dbPool $ Hs.statement opts.statusDC Ss.getStatusByName
      case derefValues rezB rezC rezD rezE of
        Left err -> pure . Left $ err
        Right (domainID, typeID, tierID, statusID) ->
          let
            derefIDs = rights [rezB, rezC, rezD, rezE]
          in
          case filter isNothing derefIDs of
            missed@(h:t) -> pure . Left $ "@[createKmsHbDoc] err label deref: " <> show derefIDs
            _ ->
              let
                actor = userID -- created_by
                code = opts.code
                title = opts.docTitle
                residency = opts.residencyDC
                aiAllowed = opts.aiAllowedDC
                legalHold = opts.legalHoldDC
                due = Nothing
              in do
              rezB <- So.createDoc dbPool actor code title domainID typeID tierID statusID (Just ownerID) residency aiAllowed legalHold due
              case rezB of
                Left err -> pure $ Left $ show err
                Right docID ->
                  let
                    principal = "user"
                    rights = V.fromList ["edit"]
                  in do
                  rezC <- So.addAclIO dbPool actor docID principal (Just ownerID) Nothing Nothing Nothing rights Nothing Nothing
                  case rezC of
                    Left err -> pure . Left $ show err
                    Right _ -> pure . Right $ InfoCR docID


derefUsers :: Either String (Maybe HbT.User) -> Either String (Maybe HbT.User) -> Either String (Int32, Int32)
derefUsers eiMbUser eiMbOwner =
  case (eiMbUser, eiMbOwner) of
    (Right mbUser, Right mbOwner) ->
      case (mbUser, mbOwner) of
        (Just user, Just owner) -> Right (user.uidUsr, owner.uidUsr)
        _ -> Left $ "@[derefUser] err missing deref: " <> show (mbUser, mbOwner)
    _ -> Left $ "@[derefUser] err db issue: " <> show (eiMbUser, eiMbOwner)


derefValues :: Either UsageError (Maybe Int32) -> Either UsageError (Maybe Int32)
        -> Either UsageError (Maybe Int32) -> Either UsageError (Maybe Int32)
        -> Either String (Int32, Int32, Int32, Int32)
derefValues eiMbDomain eiMbType eiMbTier eiMbStatus =
  case (eiMbDomain, eiMbType, eiMbTier, eiMbStatus) of
    (Right mbDomain, Right mbType, Right mbTier, Right mbStatus) ->
      case (mbDomain, mbType, mbTier, mbStatus) of
        (Just domain, Just dType, Just tier, Just status) -> Right (domain, dType, tier, status)
        _ -> Left $ "@[derefValues] err missing deref: " <> show (mbDomain, mbType, mbTier, mbStatus)
    _ -> Left $ "@[derefValues] err db issue: " <> show (eiMbDomain, eiMbType, eiMbTier, eiMbStatus)

{-
addAclIO :: Pool -> Int32 -> Int32 -> Text -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> V.Vector Text -> Maybe Text -> Maybe Text -> IO (DataResult Int32)
addAclIO pool actor docId principal u g r o rights scope scopeVal =
-}

deleteKmsHbDoc :: Text -> Pool -> IO (Either String CmdResult)
deleteKmsHbDoc key dbPool =
  pure $ Right SuccessCR

getKmsHbDoc :: Ot.KmsGetOpts -> Pool -> IO (Either String CmdResult)
getKmsHbDoc opts dbPool =
  pure $ Right SuccessCR


-- DB ops:
listAllDocs :: Pool -> IO (Either String CmdResult)
listAllDocs dbPool = do
  rezA <- use dbPool $ Hs.statement (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) Ss.qListDocs
  case rezA of
    Left err -> pure $ Left $ show err
    Right rez -> do
      mapM_ showHbDoc rez
      pure $ Right SuccessCR


listDocsByKey :: Text -> Pool -> IO (Either String CmdResult)
listDocsByKey key dbPool = do
  rezA <- use dbPool $ Hs.statement (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) Ss.qListDocs
  case rezA of
    Left err -> pure $ Left $ show err
    Right rez -> do
      mapM_ showHbDoc rez
      pure $ Right SuccessCR

listDocsByTitle :: Text -> Pool -> IO (Either String CmdResult)
listDocsByTitle title dbPool = do
  rezA <- use dbPool $ Hs.statement (Nothing, Nothing, Nothing, Just title, Nothing, Nothing) Ss.qListDocs
  case rezA of
    Left err -> pure $ Left $ show err
    Right rez -> do
      mapM_ showHbDoc rez
      pure $ Right SuccessCR


showHbDoc :: HbT.DocRow -> IO ()
showHbDoc = print