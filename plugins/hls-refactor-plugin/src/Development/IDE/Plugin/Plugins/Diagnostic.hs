module Development.IDE.Plugin.Plugins.Diagnostic (
  matchVariableNotInScope,
  matchRegexUnifySpaces,
  unifySpaces,
  matchFoundHole,
  matchFoundHoleIncludeUnderscore,
  )
  where

import           Data.Bifunctor  (Bifunctor (..))
import qualified Data.Text       as T
import           Text.Regex.TDFA ((=~~))

unifySpaces :: T.Text -> T.Text
unifySpaces    = T.unwords . T.words

-- | Returns Just (the submatches) for the first capture, or Nothing.
matchRegex :: T.Text -> T.Text -> Maybe [T.Text]
matchRegex message regex = case message =~~ regex of
    Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, bindings) -> Just bindings
    Nothing                                                -> Nothing

-- | 'matchRegex' combined with 'unifySpaces'
--
-- >>> matchRegexUnifySpaces  "hello I'm a cow" "he(ll)o"
-- Just ["ll"]
matchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexUnifySpaces message = matchRegex (unifySpaces message)

matchFoundHole :: T.Text -> Maybe (T.Text, T.Text)
matchFoundHole message
  | Just [name, typ] <- matchRegexUnifySpaces message "Found hole: _([^ ]+) :: ([^*•]+) Or perhaps" =
      Just (name, typ)
  | otherwise = Nothing

matchFoundHoleIncludeUnderscore :: T.Text -> Maybe (T.Text, T.Text)
matchFoundHoleIncludeUnderscore message = first ("_" <>) <$> matchFoundHole message

matchVariableNotInScope :: T.Text -> Maybe (T.Text, Maybe T.Text)
matchVariableNotInScope message
  --     * Variable not in scope:
  --         suggestAcion :: Maybe T.Text -> Range -> Range
  --     * Variable not in scope:
  --         suggestAcion
  | Just (name, typ) <- matchVariableNotInScopeTyped message = Just (name, Just typ)
  | Just name <- matchVariableNotInScopeUntyped message = Just (name, Nothing)
  | otherwise = Nothing
  where
    matchVariableNotInScopeTyped message
      | Just [name, typ0] <- matchRegexUnifySpaces message "Variable not in scope: ([^ ]+) :: ([^*•]+)"
      , -- When some name in scope is similar to not-in-scope variable, the type is followed by
        -- "Suggested fix: Perhaps use ..."
        typ:_ <- T.splitOn " Suggested fix:" typ0 =
          Just (name, typ)
      | otherwise = Nothing
    matchVariableNotInScopeUntyped message
      | Just [name] <- matchRegexUnifySpaces message "Variable not in scope: ([^ ]+)" =
          Just name
      | otherwise = Nothing
