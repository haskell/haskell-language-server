module Ide.Plugin.SemanticTokens.Types where


import           Development.IDE.GHC.Compat

-- data SemanticTokenType = SClass | SVariable | STypeVar | SDataCon | SNothing | SFieldName  deriving (Eq, Ord)
--
data SemanticTokenType =
    TVariable -- fallback to variable for all other cases
    | TTypeVariable -- Type variable
    -- by match
    | TPatternBind -- PatternBind, parameters, as values in let/in, case, lambda
    | TValBind -- MatchBind, valBind instance bind or regular bind
    | TRecField -- from match bind
    | TPatternSyn -- Pattern synonym

    | TClassMethod -- Class method
    -- by decls
    | TTypeSyn -- Type synonym (Non-local is not captured)
    | TClass -- Class (Constraint constructor)
    | TTypeCon -- Type (Type constructor)
    | TDataCon -- Data constructor
    | TTypeFamily -- type family
    | TNothing -- type family
    deriving (Eq, Ord, Show, Enum)

-- instance Show SemanticTokenType where
--     show SClass     = "class"
--     show SVariable  = "SVariable"
--     show STypeVar   = "STypeVar"
--     show SDataCon   = "SDataCon"
--     show SFieldName = "SFieldName"
--     show SNothing   = "SNothing"



		-- capability.tokenTypes = [
    	-- 	SemanticTokenTypes.namespace,
    	-- 	SemanticTokenTypes.type,
    	-- 	SemanticTokenTypes.class,
    	-- 	SemanticTokenTypes.enum,
    	-- 	SemanticTokenTypes.interface,
    	-- 	SemanticTokenTypes.struct,
    	-- 	SemanticTokenTypes.typeParameter,
    	-- 	SemanticTokenTypes.parameter,
    	-- 	SemanticTokenTypes.variable,
    	-- 	SemanticTokenTypes.property,
    	-- 	SemanticTokenTypes.enumMember,
    	-- 	SemanticTokenTypes.event,
    	-- 	SemanticTokenTypes.function,
    	-- 	SemanticTokenTypes.method,
    	-- 	SemanticTokenTypes.macro,
    	-- 	SemanticTokenTypes.keyword,
    	-- 	SemanticTokenTypes.modifier,
    	-- 	SemanticTokenTypes.comment,
    	-- 	SemanticTokenTypes.string,
    	-- 	SemanticTokenTypes.number,
    	-- 	SemanticTokenTypes.regexp,
    	-- 	SemanticTokenTypes.operator,
		-- 	SemanticTokenTypes.decorator
		-- ];

		-- capability.tokenModifiers = [
    	-- 	SemanticTokenModifiers.declaration,
    	-- 	SemanticTokenModifiers.definition,
    	-- 	SemanticTokenModifiers.readonly,
    	-- 	SemanticTokenModifiers.static,
    	-- 	SemanticTokenModifiers.deprecated,
    	-- 	SemanticTokenModifiers.abstract,
    	-- 	SemanticTokenModifiers.async,
    	-- 	SemanticTokenModifiers.modification,
    	-- 	SemanticTokenModifiers.documentation,
    	-- 	SemanticTokenModifiers.defaultLibrary
-- 		-- ];
-- classToken = 2
-- variableToken = 8
-- -- enumToken = 4
-- typeParameter = 6
-- enumMember = 10
-- property = 9


-- instance Enum SemanticTokenType where
--   toEnum 2  = SClass -- class
--   toEnum 8  = SVariable -- variable
--   toEnum 6  = STypeVar -- typeParameter
--   toEnum 10 = SDataCon -- enumMember
--   toEnum 9  = SFieldName -- property
--   toEnum 14 = SNothing -- comment
--   toEnum x  = error  $ "SemanticTokenType.toEnum: bad argument:" ++ show x

--   fromEnum SClass     = classToken
--   fromEnum SVariable  = variableToken
--   fromEnum STypeVar   = typeParameter
--   fromEnum SDataCon   = enumMember
--   fromEnum SFieldName = property
--   fromEnum SNothing   = 14

type SemanticCollectFinal = (SemanticTokenType, LIdP GhcRn)
-- { line: 2, startChar 5, length: 3, tokenType: SemanticTokenType, tokenModifiers: 3, string},
type SemanticToken = (SemanticTokenData, SemanticCollectFinal)
type SemanticTokenData = (Int, Int, Int, Int, Int)
type SemanticTokenInt = Int

data SemanticTokenOriginal =  SemanticTokenOriginal
  { tokenType :: SemanticTokenType,
    loc       :: Loc,
    name      :: String
  }
  deriving (Show, Eq, Ord)

data Loc = Loc
  { line      :: Int,
    startChar :: Int,
    len       :: Int
  }
  deriving (Show, Eq, Ord)
