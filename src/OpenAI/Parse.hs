module OpenAI.Parse where

import qualified Data.List as L
import qualified Data.Map.Strict as Mp


import qualified Options.Runtime as Rto
import qualified OpenAI.Json.Reader as Jd


analyzeDiscussion :: Jd.Discussion -> IO ()
analyzeDiscussion discussion =
  let
    rootChild = Mp.lookup "client-created-root" discussion.mappingCv
  in
  case rootChild of
    Just node -> mapM_ (analyseChild discussion.mappingCv 0) node.childrenNd
    Nothing -> putStrLn "No root child found"


analyseChild :: Mp.Map String Jd.Node -> Int -> String -> IO ()
analyseChild mapping depth target = do
  case Mp.lookup target mapping of
    Just child -> do
      showChildContent depth child
      mapM_ (analyseChild mapping (depth + 1)) child.childrenNd
    Nothing -> putStrLn $ "@[analyseChild] no child: " ++ target


showChildContent :: Int -> Jd.Node -> IO ()
showChildContent depth child =
  let
    prefix = replicate depth ' '
  in do
  case child.messageNd of
    Just message ->
      case message.authorMsg.roleAu of
        "user" -> do
          case message.contentMsg of
            Jd.TextContent parts ->
              case parts of
                [] -> pure ()
                [""] -> pure ()
                _ -> putStrLn $ "\n*** USER: " <> L.intercalate ", " parts <> "\n"
            _ -> pure ()
            {-
            case Mp.lookup "reasoning_status" message.metadataMsg of

              "finish_details" -> do
                  putStrLn "Answer:"
                  case message.contentMsg of
                    Jd.TextContent parts -> case parts of
                      [] -> pure ()
                      [""] -> pure ()
                      _ -> putStrLn $ L.intercalate ", " parts

            Just aValue ->
              case aValue of
                "reasoning_ended" ->
                  case message.contentMsg of
                    Jd.TextContent parts -> case parts of
                      [] -> pure ()
                      [""] -> pure ()
                      _ -> putStrLn $ L.intercalate ", " parts
                    _ -> pure ()
                _ -> 
            Nothing -> putStrLn $ "@[showChildContent] no reasoning_status found, skipping message: " ++ message.idMsg
            -}
        "assistant" ->
          case message.endTurnMsg of
            Just True ->
              case message.contentMsg of
                Jd.TextContent parts -> case parts of
                  [] -> pure ()
                  [""] -> pure ()
                  _ -> putStrLn $ "\nANSWER:--------------------------------\n" <> L.intercalate ", " parts <> "\n--------------------------------\n"
                -- Jd.ModelEditableContext modelSetContext repository repoSummary structuredContext -> putStrLn $ "ModelEditableContext: " ++ modelSetContext
                Jd.ThoughtsContent thoughts sourceAnalysisMsgId ->
                  mapM_ (\thought -> putStrLn $ prefix <> "Thought: " <> thought.summaryTh) thoughts
                -- Jd.CodeContent language responseFormatName text -> putStrLn $ "CodeContent: " ++ text
                _ -> pure ()
            Just False ->
               case message.contentMsg of
                 Jd.TextContent parts -> case parts of
                   [] -> pure ()
                   [""] -> pure ()
                   _ -> putStrLn $ "*** ANSWER: " <> L.intercalate ", " parts
                 -- Jd.ModelEditableContext modelSetContext repository repoSummary structuredContext -> putStrLn $ "ModelEditableContext: " ++ modelSetContext
                 Jd.ThoughtsContent thoughts sourceAnalysisMsgId ->
                    mapM_ (\thought -> putStrLn $ prefix <> "Thought: " <> thought.summaryTh) thoughts
                 -- Jd.CodeContent language responseFormatName text -> putStrLn $ "CodeContent: " ++ text
                 _ -> pure ()
            Nothing -> pure ()
        "system" ->
          case message.authorMsg.nameAu of
            Just name ->
              case name of
                "web.run" -> putStrLn "@[showChildContent] web.run"
                "python" -> putStrLn "@[showChildContent] python"
                _ -> putStrLn $ "@[showChildContent] unknown author: " <> name
            Nothing -> pure () --putStrLn $ "@[showChildContent] no name found, skipping message: " ++ message.idMsg
        "tool" -> putStrLn $ prefix <> "Tool: TODO"
        _ -> putStrLn $ "@[showChildContent] unknown author: " <> message.authorMsg.roleAu
    -- No child.messageNd:
    Nothing -> putStrLn $ "@[showChildContent] skipping: " <> child.idNd