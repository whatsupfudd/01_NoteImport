module OpenAI.Utils where

import Data.Either (lefts, rights)
import Data.Scientific (Scientific, fromFloatDigits)

import qualified Hasql.Pool as Hp


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


safeScientific :: Double -> Maybe Scientific
safeScientific aVal
  | isNaN aVal = Nothing
  | isInfinite aVal = Nothing
  | otherwise  = Just (fromFloatDigits aVal)

