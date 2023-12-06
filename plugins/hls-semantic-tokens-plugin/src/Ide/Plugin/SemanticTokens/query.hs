module Ide.Plugin.SemanticTokens.Query where
import           Data.Map (Map)


-- type IdentifierMap = Map (Set ContextInfo)

-- | Recurses through the given AST to find identifiers which are
-- 'InstanceValBind's.
-- identifierGetter :: HieAST a -> [(Span, Identifier, IdentifierDetails a)]
-- identifierGetter ast =
--     let ids = [ (nodeSpan ast, c, d) | (c, d) <- Map.toList $ getNodeIds ast]
--     in ids <> concatMap identifierGetter (nodeChildren ast)



