{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module HBDoc.Structure where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime, Day)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

-- Lookup rows
data Domain = Domain {
      uidDom :: !Int32
    , codeDom :: !Text
    , nameDom :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data DocType = DocType {
    uidDct :: !Int32
  , codeDct :: !Text
  , nameDct :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Tier = Tier {
    uidTr :: !Int32
  , codeTr :: !Text
  , nameTr :: !Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Status = Status {
    uidSt :: !Int32
  , codeSt :: !Text
  , nameSt :: !Text
  , orderSt :: !Int32
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Minimal user projection
data User = User { 
    uidUsr :: !Int32
  , emailUsr :: !Text
  , fullNameUsr :: !Text
  , isExternalUsr :: !Bool
  , orgNameUsr  :: !(Maybe Text)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- List row for documents
data DocRow = DocRow { 
    uidDoc      :: !Int32
  , titleDoc    :: !Text
  , domainCode' :: !Text
  , typeCodeDoc   :: !Text
  , tierCodeDoc   :: !Text
  , statusCodeDoc :: !Text
  , dueDateDoc    :: !(Maybe Day)
  , updatedAtDoc  :: !UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Document detail (summary + latest version id if present)
data DocDetail = DocDetail { 
    uidDtl         :: !Int32
  , titleDtl       :: !Text
  , domainFkDtl    :: !Int32
  , typeFkDtl      :: !Int32
  , tierFkDtl      :: !Int32
  , statusFkDtl    :: !Int32
  , ownerUserFkDtl :: !(Maybe Int32)
  , residencyDtl   :: !(Maybe Text)
  , aiAllowedDtl   :: !Bool
  , legalHoldDtl   :: !Bool
  , dueDateDtl     :: !(Maybe Day)
  , createdAtDtl   :: !UTCTime
  , updatedAtDtl   :: !UTCTime
  , latestVerUidDtl :: !(Maybe Int32)
  , latestVerNoDtl  :: !(Maybe Int32)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


data Category = Category {
    uidCat :: !Int32
  , nameCat :: !Text
  , parentCat :: !(Maybe Int32)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- Version snapshot
data DocVersion = DocVersion { 
    uidVer        :: !Int32
  , docFkVer      :: !Int32
  , noVer         :: !Int32
  , noteVer       :: !(Maybe Text)
  , contentRefVer :: !(Maybe Text)
  , contentShaVer :: !(Maybe Text)
  , createdByVer  :: !(Maybe Int32)
  , createdAtVer  :: !UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- Comment
data Comment = Comment { 
    uidCmt     :: !Int32
  , docFkCmt   :: !Int32
  , parentCmt  :: !(Maybe Int32)
  , authorCmt  :: !(Maybe Int32)
  , bodyCmt    :: !Text
  , resolvedCmt:: !Bool
  , createdCmt :: !UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ACL entry
data AclEntry = AclEntry { 
    uidAcl :: !Int32
  , docFkAcl :: !Int32
  , principalAcl :: !Text    -- 'user' | 'group' | 'role' | 'org'
  , userFkAcl :: !(Maybe Int32)
  , groupFkAcl :: !(Maybe Int32)
  , roleFkAcl :: !(Maybe Int32)
  , orgFkAcl :: !(Maybe Int32)
  , rightsAcl :: !(Vector Text)
  , scopeAcl :: !(Maybe Text) -- scope_kind
  , xcopeValAcl :: !(Maybe Text)
  , byAcl :: !(Maybe Int32)
  , atAcl :: !UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- Report cells
data CountCell = CountCell {
    keyACC:: !Int32
    , keyBCC :: !Int32
    , cntCC :: !Int32
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- Sankey link counts
data SankeyAB = SankeyAB {
    codeASK :: !Text
    , codeBSK :: !Text
    , countABSK :: !Int32
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- Generic API result wrapper
data ApiResult a = ApiResult {
    okAR :: !Bool
    , resultAR :: !(Maybe a)
    , errAR :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


data BlockKind =
  DocumentBK
  | HeadingBK Int32 -- level 1..6
  | ParagraphBK
  | RunBK
  | ListItemBK Int32 -- nesting level 0..N
  | TableBK
  | TableRowBK
  | TableCellBK
  | ImageBK -- image ref fills attrs
  | FootnoteBK
  | EndnoteBK
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


data Block = Block { 
    uidBlk     :: !Int32            -- simple incremental id within a version
  , kindBlk   :: !BlockKind
  , textBlk   :: !(Maybe Text)    -- normalized text content
  , attrsBlk  :: !(Maybe ObjectAttrs)
  , kidsBlk   :: ![Block]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- extra attributes (style, numbering, refs, image keys, etc.)
data ObjectAttrs = ObjectAttrs { 
    styleNameOA  :: !(Maybe Text)
  , styleIdOA    :: !(Maybe Text)
  , numIdOA      :: !(Maybe Int32)
  , ilvlOA       :: !(Maybe Int32)
  , numFmtOA     :: !(Maybe Text)
  , lvlTextOA    :: !(Maybe Text)
  , startAtOA    :: !(Maybe Int32)
  , captionOA    :: !(Maybe Text)
  , imageRidOA   :: !(Maybe Text)     -- relationship id for images
  , imageNameOA  :: !(Maybe Text)
  , imageKeyOA   :: !(Maybe Text)     -- object store key for binary
  , widthEmuOA   :: !(Maybe Int32)      -- optional: wp:extent/@cx (EMU)
  , heightEmuOA  :: !(Maybe Int32)      -- optional: wp:extent/@cy (EMU)
  , noteIdOA     :: !(Maybe Int32)
  , noteTypeOA   :: !(Maybe Text)   -- "footnote" | "endnote"
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Result of an import
data ImportResult = ImportResult { 
    attachmentUidIR   :: !Int32 -- attachment.uid of source DOCX
  , versionUidIR      :: !Int32 -- document_version.uid for parsed tree
  , versionNoIR       :: !Int32
  , contentRefIR      :: !Text -- object key for JSON block tree
  , textCharsIR       :: !Int32 -- number of characters in plaintext
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

