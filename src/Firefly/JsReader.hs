{-# LANGUAGE DerivingStrategies #-}

module Firefly.JsReader where

import Data.Scientific (Scientific)
import Data.Text (Text)

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), Value)


data Root = Root {
    dataRoot :: Data
  }
  deriving stock (Eq, Show)

instance FromJSON Root where
  parseJSON = withObject "Root" $ \o ->
    Root
      <$> o .: "data"


data Data = Data {
    meetingNoteData :: MeetingNoteData
  }
  deriving stock (Eq, Show)

instance FromJSON Data where
  parseJSON = withObject "Data" $ \o ->
    Data
      <$> o .: "meetingNote"


data MeetingNoteData = MeetingNoteData {
    typenameMeetingNoteData :: Text
    , idMeetingNoteData :: Text
    , addedByMeetingNoteData :: Maybe Value
    , allEmailsMeetingNoteData :: Maybe Value
    , allEmailsProfilesMeetingNoteData :: [Value]
    , allEmailsSentToMeetingNoteData :: Maybe Value
    , attendeesMeetingNoteData :: [Value]
    , audioOnlyMeetingNoteData :: Bool
    , audioServiceMetadataMeetingNoteData :: AudioServiceMetadataDataMeetingNote
    , audioUrlMeetingNoteData :: Text
    , captionsMeetingNoteData :: [CaptionDataMeetingNote]
    , channelMeetingNoteData :: Maybe Value
    , channelsMeetingNoteData :: [Value]
    , createdAtMeetingNoteData :: Text
    , creatorEmailMeetingNoteData :: Text
    , dateMeetingNoteData :: Text
    , defaultChannelAccessMeetingNoteData :: Bool
    , durationMinsMeetingNoteData :: Text
    , expandSummaryItemCountMeetingNoteData :: Maybe Value
    , hasCaptionsMeetingNoteData :: Bool
    , isGuestAccessEnabledMeetingNoteData :: Bool
    , isOnPrivateStorageMeetingNoteData :: Bool
    , isVideoAvailableMeetingNoteData :: Bool
    , joinedUsersMeetingNoteData :: Maybe Value
    , labelMetaMeetingNoteData :: LabelMetaDataMeetingNote
    , manualNotesMeetingNoteData :: Maybe Value
    , meetingExpirySettingsMeetingNoteData :: Maybe Value
    , ownerMeetingNoteData :: Text
    , ownerProfileMeetingNoteData :: OwnerProfileDataMeetingNote
    , ownersListMeetingNoteData :: Maybe Value
    , paragraphMetaMeetingNoteData :: ParagraphMetaDataMeetingNote
    , parseIdMeetingNoteData :: Text
    , privacyMeetingNoteData :: Text
    , privacyListMeetingNoteData :: Maybe Value
    , processMeetingStatusMeetingNoteData :: Text
    , puppetStatesMeetingNoteData :: Maybe Value
    , recordingPreferenceMeetingNoteData :: Maybe Value
    , retranscribeCountMeetingNoteData :: Maybe Value
    , sentenceMetaMeetingNoteData :: SentenceMetaDataMeetingNote
    , speakerMetaMeetingNoteData :: SpeakerMetaDataMeetingNote
    , summaryMeetingNoteData :: SummaryDataMeetingNote
    , summaryStatusMeetingNoteData :: Text
    , titleMeetingNoteData :: Text
    , topicMetaMeetingNoteData :: TopicMetaDataMeetingNote
    , transcriptParseIdMeetingNoteData :: Maybe Value
    , txServiceMeetingNoteData :: Maybe Value
    , userPermissionMeetingNoteData :: Text
    , videoUrlMeetingNoteData :: Maybe Value
    , waveformDataMeetingNoteData :: Maybe Value
  }
  deriving stock (Eq, Show)

instance FromJSON MeetingNoteData where
  parseJSON = withObject "MeetingNoteData" $ \o ->
    MeetingNoteData
      <$> o .: "__typename"
      <*> o .: "_id"
      <*> o .: "addedBy"
      <*> o .: "allEmails"
      <*> o .: "allEmailsProfiles"
      <*> o .: "allEmailsSentTo"
      <*> o .: "attendees"
      <*> o .: "audioOnly"
      <*> o .: "audioServiceMetadata"
      <*> o .: "audio_url"
      <*> o .: "captions"
      <*> o .: "channel"
      <*> o .: "channels"
      <*> o .: "createdAt"
      <*> o .: "creator_email"
      <*> o .: "date"
      <*> o .: "defaultChannelAccess"
      <*> o .: "durationMins"
      <*> o .: "expandSummaryItemCount"
      <*> o .: "hasCaptions"
      <*> o .: "isGuestAccessEnabled"
      <*> o .: "isOnPrivateStorage"
      <*> o .: "isVideoAvailable"
      <*> o .: "joinedUsers"
      <*> o .: "labelMeta"
      <*> o .: "manualNotes"
      <*> o .: "meetingExpirySettings"
      <*> o .: "owner"
      <*> o .: "ownerProfile"
      <*> o .: "ownersList"
      <*> o .: "paragraphMeta"
      <*> o .: "parseId"
      <*> o .: "privacy"
      <*> o .: "privacyList"
      <*> o .: "processMeetingStatus"
      <*> o .: "puppetStates"
      <*> o .: "recordingPreference"
      <*> o .: "retranscribeCount"
      <*> o .: "sentenceMeta"
      <*> o .: "speakerMeta"
      <*> o .: "summary"
      <*> o .: "summaryStatus"
      <*> o .: "title"
      <*> o .: "topicMeta"
      <*> o .: "transcriptParseId"
      <*> o .: "txService"
      <*> o .: "userPermission"
      <*> o .: "video_url"
      <*> o .: "waveformData"


data AudioServiceMetadataDataMeetingNote = AudioServiceMetadataDataMeetingNote {
    typenameAudioServiceMetadataDataMeetingNote :: Text
    , hasGeneratedInstantSummaryAudioServiceMetadataDataMeetingNote :: Maybe Value
    , hasManualNotesMergedAudioServiceMetadataDataMeetingNote :: Maybe Value
    , languageCodeAudioServiceMetadataDataMeetingNote :: Text
    , numCaptionsAudioServiceMetadataDataMeetingNote :: Integer
    , preferredLanguageAudioServiceMetadataDataMeetingNote :: Text
    , silentMeetingAudioServiceMetadataDataMeetingNote :: Bool
    , skipSummaryReasonAudioServiceMetadataDataMeetingNote :: Maybe Value
  }
  deriving stock (Eq, Show)

instance FromJSON AudioServiceMetadataDataMeetingNote where
  parseJSON = withObject "AudioServiceMetadataDataMeetingNote" $ \o ->
    AudioServiceMetadataDataMeetingNote
      <$> o .: "__typename"
      <*> o .: "hasGeneratedInstantSummary"
      <*> o .: "hasManualNotesMerged"
      <*> o .: "languageCode"
      <*> o .: "numCaptions"
      <*> o .: "preferredLanguage"
      <*> o .: "silentMeeting"
      <*> o .: "skipSummaryReason"


data CaptionDataMeetingNote = CaptionDataMeetingNote {
    typenameCaptionDataMeetingNote :: Text
    , endTimeCaptionDataMeetingNote :: Scientific
    , filterTypeCaptionDataMeetingNote :: Text
    , indexCaptionDataMeetingNote :: Integer
    , matchCaptionDataMeetingNote :: Text
    , metricsCaptionDataMeetingNote :: Maybe [MetricDataMeetingNoteCaption]
    , sentenceCaptionDataMeetingNote :: Text
    , sentimentCaptionDataMeetingNote :: Maybe Scientific
    , sentimentTypeCaptionDataMeetingNote :: Text
    , speakerIdCaptionDataMeetingNote :: Integer
    , timeCaptionDataMeetingNote :: Scientific
  }
  deriving stock (Eq, Show)

instance FromJSON CaptionDataMeetingNote where
  parseJSON = withObject "CaptionDataMeetingNote" $ \o ->
    CaptionDataMeetingNote
      <$> o .: "__typename"
      <*> o .: "endTime"
      <*> o .: "filterType"
      <*> o .: "index"
      <*> o .: "match"
      <*> o .: "metrics"
      <*> o .: "sentence"
      <*> o .: "sentiment"
      <*> o .: "sentimentType"
      <*> o .: "speaker_id"
      <*> o .: "time"


data MetricDataMeetingNoteCaption = MetricDataMeetingNoteCaption {
    typenameMetricDataMeetingNoteCaption :: Text
    , categoryMetricDataMeetingNoteCaption :: Text
    , wordMetricDataMeetingNoteCaption :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON MetricDataMeetingNoteCaption where
  parseJSON = withObject "MetricDataMeetingNoteCaption" $ \o ->
    MetricDataMeetingNoteCaption
      <$> o .: "__typename"
      <*> o .: "category"
      <*> o .: "word"


data LabelMetaDataMeetingNote = LabelMetaDataMeetingNote
  deriving stock (Eq, Show)

instance FromJSON LabelMetaDataMeetingNote where
  parseJSON = withObject "LabelMetaDataMeetingNote" $ \_ -> pure LabelMetaDataMeetingNote


data OwnerProfileDataMeetingNote = OwnerProfileDataMeetingNote {
    typenameOwnerProfileDataMeetingNote :: Text
    , canUseAICreditsOwnerProfileDataMeetingNote :: Bool
    , emailOwnerProfileDataMeetingNote :: Text
    , nameOwnerProfileDataMeetingNote :: Text
    , pictureOwnerProfileDataMeetingNote :: Text
    , tierOwnerProfileDataMeetingNote :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON OwnerProfileDataMeetingNote where
  parseJSON = withObject "OwnerProfileDataMeetingNote" $ \o ->
    OwnerProfileDataMeetingNote
      <$> o .: "__typename"
      <*> o .: "canUseAICredits"
      <*> o .: "email"
      <*> o .: "name"
      <*> o .: "picture"
      <*> o .: "tier"


data ParagraphMetaDataMeetingNote = ParagraphMetaDataMeetingNote
  deriving stock (Eq, Show)

instance FromJSON ParagraphMetaDataMeetingNote where
  parseJSON = withObject "ParagraphMetaDataMeetingNote" $ \_ -> pure ParagraphMetaDataMeetingNote


data SentenceMetaDataMeetingNote = SentenceMetaDataMeetingNote
  deriving stock (Eq, Show)

instance FromJSON SentenceMetaDataMeetingNote where
  parseJSON = withObject "SentenceMetaDataMeetingNote" $ \_ -> pure SentenceMetaDataMeetingNote


data SpeakerMetaDataMeetingNote = SpeakerMetaDataMeetingNote {
    n1SpeakerMetaDataMeetingNote :: Text
    , n2SpeakerMetaDataMeetingNote :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON SpeakerMetaDataMeetingNote where
  parseJSON = withObject "SpeakerMetaDataMeetingNote" $ \o ->
    SpeakerMetaDataMeetingNote
      <$> o .: "1"
      <*> o .: "2"


data SummaryDataMeetingNote = SummaryDataMeetingNote {
    typenameSummaryDataMeetingNote :: Text
    , gistSummaryDataMeetingNote :: Text
    , shortSummarySummaryDataMeetingNote :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON SummaryDataMeetingNote where
  parseJSON = withObject "SummaryDataMeetingNote" $ \o ->
    SummaryDataMeetingNote
      <$> o .: "__typename"
      <*> o .: "gist"
      <*> o .: "shortSummary"


data TopicMetaDataMeetingNote = TopicMetaDataMeetingNote
  deriving stock (Eq, Show)

instance FromJSON TopicMetaDataMeetingNote where
  parseJSON = withObject "TopicMetaDataMeetingNote" $ \_ -> pure TopicMetaDataMeetingNote

