{-# LANGUAGE DerivingStrategies #-}

module OpenAI.Content.Kind (KindPart(..), textKC, kindFromText, kindFromJson, textPP, partFromText) where

import Data.Text (Text)
import qualified Data.Text as T

import OpenAI.Content.Types (IssueC(..), KindC(..), PartPL(..))
import qualified OpenAI.Json.Reader as Jd


data KindPart
  = TextKP
  | AudioTransKP
  | AudioAssetKP
  | ImageAssetKP
  | RealtimeAvKP
  | OtherKP Text
  deriving stock (Eq, Ord, Show)


textKC :: KindC -> Text
textKC kind =
  case kind of
    CodeKC -> "code"
    ExecOutKC -> "execution_output"
    ModelCtxKC -> "model_editable_context"
    MultiTextKC -> "multimodal_text"
    ReasoningKC -> "reasoning_recap"
    SystemErrKC -> "system_error"
    TetherBrowseKC -> "tether_browsing_display"
    TetherQuoteKC -> "tether_quote"
    TextKC -> "text"
    ThoughtsKC -> "thoughts"
    OtherKC kindTxt -> kindTxt


kindFromText :: Text -> KindC
kindFromText kindTxt =
  case kindTxt of
    "code" -> CodeKC
    "execution_output" -> ExecOutKC
    "model_editable_context" -> ModelCtxKC
    "multimodal_text" -> MultiTextKC
    "reasoning_recap" -> ReasoningKC
    "system_error" -> SystemErrKC
    "tether_browsing_display" -> TetherBrowseKC
    "tether_quote" -> TetherQuoteKC
    "text" -> TextKC
    "thoughts" -> ThoughtsKC
    _ -> OtherKC kindTxt


kindFromJson :: Jd.Content -> KindC
kindFromJson content =
  case content of
    Jd.CodeCT {} -> CodeKC
    Jd.ExecutionOutputCT {} -> ExecOutKC
    Jd.ModelEditableContextCT {} -> ModelCtxKC
    Jd.MultimodalTextCT {} -> MultiTextKC
    Jd.ReasoningRecapCT {} -> ReasoningKC
    Jd.SystemErrorCT {} -> SystemErrKC
    Jd.TetherBrowsingDisplayCT {} -> TetherBrowseKC
    Jd.TetherQuoteCT {} -> TetherQuoteKC
    Jd.TextCT {} -> TextKC
    Jd.ThoughtsCT {} -> ThoughtsKC
    Jd.OtherCT {Jd.contentTypeOc = kindTxt} -> OtherKC kindTxt


textPP :: PartPL -> Text
textPP part =
  case part of
    TextPP {} -> "text"
    AudioTransPP {} -> "audio_transcription"
    AudioAssetPP {} -> "audio_asset_pointer"
    ImageAssetPP {} -> "image_asset_pointer"
    RealtimeAvPP {} -> "real_time_user_audio_video"


partFromText :: Text -> Either IssueC KindPart
partFromText kindTxt =
  case kindTxt of
    "text" -> Right TextKP
    "audio_transcription" -> Right AudioTransKP
    "audio_asset_pointer" -> Right AudioAssetKP
    "image_asset_pointer" -> Right ImageAssetKP
    "real_time_user_audio_video" -> Right RealtimeAvKP
    _
      | T.null $ T.strip kindTxt -> Left $ UnknownKindIC kindTxt
      | otherwise -> Right $ OtherKP kindTxt