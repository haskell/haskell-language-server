module Ide.Plugin.Cabal.Cabal where

import qualified Data.Map                                    as Map
import           Data.Maybe                                  (isJust)
import qualified Data.Text                                   as T
import           Ide.Plugin.Cabal.Completion.Completer.Types
import           Ide.Plugin.Cabal.Completion.Data
import qualified Language.LSP.Protocol.Types                 as P (Position (..))

import           Ide.Plugin.Cabal.Completion.Types

stanzaMapFrom :: T.Text -> Maybe (Map.Map KeyWordName Completer)
stanzaMapFrom s = Map.lookup s stanzaKeywordMap

isStanzaType :: T.Text -> Bool
isStanzaType s = isJust $ Map.lookup s stanzaKeywordMap

mkPosition :: Int -> Int -> P.Position
mkPosition lineNumber charNumber = P.Position{P._line = fromIntegral lineNumber, P._character = fromIntegral charNumber}
