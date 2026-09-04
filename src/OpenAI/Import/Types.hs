{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Import.Types
  ( Mode(..)
  , Scope(..)
  , Policy(..)
  , Opts(..)
  , Step(..)
  , StepSet(..)
  , ConvState(..)
  , Action(..)
  , Source(..)
  , Ref(..)
  ) where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)

data Mode
  = WriteM
  | DryM
  deriving stock (Eq, Ord, Show)


data Scope =
    RawOnlyS
  | RawDiscS
  | RawDiscSummaryS
  deriving stock (Eq, Ord, Show)


data Policy = Policy {
    allowOlderPol :: Bool
  , allowRepairPol :: Bool
  , stopOnFailPol :: Bool
  }
  deriving stock (Eq, Show)


data Opts = Opts {
    modeOpt :: Mode
  , scopeOpt :: Scope
  , policyOpt :: Policy
  }
  deriving stock (Eq, Show)


data Step =
    RawSP
  | DiscSP
  | SummarySP
  deriving stock (Eq, Ord, Enum, Bounded, Show)


data StepSet = StepSet
  { rawSS :: Bool
  , discSS :: Bool
  , summarySS :: Bool
  }
  deriving stock (Eq, Show)

data ConvState
  = AbsentCS
      { eidConv :: Text
      }
  | PresentCS
      { uidConv :: Int64
      , eidConv :: Text
      , titleDb :: Text
      , timeUpdateDb :: Scientific
      }
  | OlderCS
      { uidConv :: Int64
      , eidConv :: Text
      , timeUpdateDb :: Scientific
      , timeUpdateJs :: Scientific
      }
  | BrokenCS
      { eidConv :: Text
      , issues :: [Text]
      }
  deriving stock (Eq, Show)

data Action
  = AddFreshA
  | UpdateKnownA
  | SkipSameA
  | SkipOlderA
  | FailA
  deriving stock (Eq, Ord, Show)

data Source = Source
  { pathSrc :: Maybe FilePath
  , exportSrc :: Bool
  , labelSrc :: Maybe Text
  }
  deriving stock (Eq, Show)

data Ref
  = EidR Text
  | UidR Int64
  deriving stock (Eq, Ord, Show)