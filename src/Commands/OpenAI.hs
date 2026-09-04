{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module Commands.OpenAI where

import qualified Options.Runtime as Rto
import qualified Options.Types as Opt

import qualified OpenAI.Actions as Oai

oaiCmd :: Opt.OaiSubCommand -> Rto.RunOptions -> IO ()
oaiCmd opts rtOpts =
  case opts of
    Opt.JsonSC jsonCmd ->
      case jsonCmd of
        Opt.PrintJS printOpts targetsOpts -> Oai.printJson printOpts targetsOpts
        Opt.StoreJS storeOpts targetsOpts -> Oai.storeJsonAsConversations storeOpts targetsOpts rtOpts
    Opt.SummarySC summaryOpts -> Oai.saveSummaries summaryOpts rtOpts
    Opt.DocxSC genOpts -> Oai.saveDocx genOpts rtOpts
    Opt.ProjFetchSC fetchOpts -> Oai.saveProject fetchOpts rtOpts
    Opt.ConversationSC conversationCmd ->
      case conversationCmd of
        Opt.DeserializeCS genOpts -> Oai.deserializeConversation genOpts rtOpts
        Opt.ConvertCS targetsOpts -> Oai.convertConversation targetsOpts rtOpts
        Opt.DocxCS genOpts -> Oai.saveConversationToDocx genOpts rtOpts
