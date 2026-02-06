module OpenAI.Utils where

import qualified Hasql.Pool as Hp
import Data.Either (lefts, rights)


listResultsToResultList :: [Either Hp.UsageError (Either String rezT)] -> Either [Hp.UsageError] (Either [String] [rezT])
listResultsToResultList results =
  case lefts results of
    [] ->
      let
        innerResults = rights results
      in
      case lefts innerResults of
      [] -> Right . Right $ rights innerResults
      errs -> Right $ Left errs
    errs -> Left errs
