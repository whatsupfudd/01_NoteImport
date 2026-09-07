{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Delta.Types (
  Delta(..)
  , MetaAct(..)
  , NodeAct(..)
  , MsgAct(..)
  , ContentAct(..)
  , Verdict(..)
  , Conflict(..)
  , Reason(..)
  , RefNode(..)
  , RefMsg(..)
  , Hash(..)
  , Stat(..)
  , emptyStat
) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)


data RefNode = RefNode {
    eidNode :: Text
    , uidNode :: Maybe Int64
  }
  deriving stock (Eq, Ord, Show)


data RefMsg = RefMsg {
    eidMsg :: Text
    , uidMsg :: Maybe Int64
  }
  deriving stock (Eq, Ord, Show)


newtype Hash = Hash {
    bytesHash :: ByteString
  }
  deriving stock (Eq, Ord, Show)


data MetaAct =
    KeepMeta
  | UpdateMeta {
      oldTitle :: Text
      , newTitle :: Text
      , oldTimeUpd :: Scientific
      , newTimeUpd :: Scientific
    }
  | RejectOlderMeta {
      dbTimeUpd :: Scientific
      , jsTimeUpd :: Scientific
    }
  deriving stock (Eq, Show)


data NodeAct =
    KeepNA {
      refNode :: RefNode
    }
  | AddNA {
      eidNode :: Text
      , eidParent :: Maybe Text
      , seqNode :: Int32
      , seqChild :: Int32
      , seqPre :: Int32
    }
  | MoveNA {
      refNode :: RefNode
      , eidParentOld :: Maybe Text
      , eidParentNew :: Maybe Text
      , seqChildOld :: Int32
      , seqChildNew :: Int32
      , seqPreOld :: Int32
      , seqPreNew :: Int32
    }
  | RewriteNA {
      refNode :: RefNode
      , msgAct :: Maybe MsgAct
    }
  | ConflictNA {
      refNode :: RefNode
      , conflict :: Conflict
    }
  deriving stock (Eq, Show)


data MsgAct =
    KeepMA {
      refMsg :: RefMsg
    }
  | AddMA {
      eidNode :: Text
      , eidMsg :: Text
    }
  | RewriteMA {
      refMsg :: RefMsg
      , hashOld :: Maybe Hash
      , hashNew :: Hash
      , reason :: Reason
    }
  | ConflictMA {
      refMsg :: RefMsg
      , conflict :: Conflict
    }
  deriving stock (Eq, Show)


data ContentAct =
    KeepCA
  | AddCA
  | RewriteCA {
      hashOld :: Maybe Hash
      , hashNew :: Hash
    }
  deriving stock (Eq, Show)


data Verdict =
    SameV
  | AddedV
  | ChangedV Reason
  | MissingV
  | ConflictV Conflict
  deriving stock (Eq, Show)


data Reason =
    TimeNewerR
  | HashChangedR
  | ShapeChangedR
  | ParentChangedR
  | OrderChangedR
  | PolicyR Text
  deriving stock (Eq, Show)


data Conflict =
    OlderJsonC
  | MissingDbNodeC Text
  | MissingJsonNodeC Text
  | ParentMismatchC Text
  | HashMismatchC
  | BranchRewriteC
  | DuplicateEidC Text
  | BrokenShapeC Text
  | DebugShapC Text Text
  deriving stock (Eq, Show)


data Delta = Delta {
    eidConv :: Text
    , uidConv :: Int64
    , metaAct :: MetaAct
    , nodeActs :: [NodeAct]
    , stat :: Stat
    , notes :: [Text]
  }
  deriving stock (Eq, Show)


data Stat = Stat {
    nodeAddCnt :: Int
    , nodeKeepCnt :: Int
    , nodeMoveCnt :: Int
    , nodeRewriteCnt :: Int
    , msgAddCnt :: Int
    , msgKeepCnt :: Int
    , msgRewriteCnt :: Int
    , conflictCnt :: Int
  }
  deriving stock (Eq, Show)


emptyStat :: Stat
emptyStat =
  Stat {
    nodeAddCnt = 0
    , nodeKeepCnt = 0
    , nodeMoveCnt = 0
    , nodeRewriteCnt = 0
    , msgAddCnt = 0
    , msgKeepCnt = 0
    , msgRewriteCnt = 0
    , conflictCnt = 0
  }