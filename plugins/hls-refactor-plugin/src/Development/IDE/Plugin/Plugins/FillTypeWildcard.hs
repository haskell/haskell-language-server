module Development.IDE.Plugin.Plugins.FillTypeWildcard
  ( suggestFillTypeWildcard
  ) where

import           Data.Char
import qualified Data.Text          as T
import           Language.LSP.Types (Diagnostic (..), TextEdit (TextEdit))

suggestFillTypeWildcard :: Diagnostic -> [(T.Text, TextEdit)]
suggestFillTypeWildcard Diagnostic{_range=_range,..}
-- Foo.hs:3:8: error:
--     * Found type wildcard `_' standing for `p -> p1 -> p'
    | "Found type wildcard" `T.isInfixOf` _message
    , " standing for " `T.isInfixOf` _message
    , typeSignature <- extractWildCardTypeSignature _message
        =  [("Use type signature: ‘" <> typeSignature <> "’", TextEdit _range typeSignature)]
    | otherwise = []

-- | Extract the type and surround it in parentheses except in obviously safe cases.
--
-- Inferring when parentheses are actually needed around the type signature would
-- require understanding both the precedence of the context of the hole and of
-- the signature itself. Inserting them (almost) unconditionally is ugly but safe.
extractWildCardTypeSignature :: T.Text -> T.Text
extractWildCardTypeSignature msg
  | enclosed || not isApp || isToplevelSig = sig
  | otherwise                              = "(" <> sig <> ")"
  where
    msgSigPart      = snd $ T.breakOnEnd "standing for " msg
    (sig, rest)     = T.span (/='’') . T.dropWhile (=='‘') . T.dropWhile (/='‘') $ msgSigPart
    -- If we're completing something like ‘foo :: _’ parens can be safely omitted.
    isToplevelSig   = errorMessageRefersToToplevelHole rest
    -- Parenthesize type applications, e.g. (Maybe Char).
    isApp           = T.any isSpace sig
    -- Do not add extra parentheses to lists, tuples and already parenthesized types.
    enclosed        =
      case T.uncons sig of
        Nothing -> error "GHC provided invalid type"
        Just (firstChr, _) -> not (T.null sig) && (firstChr, T.last sig) `elem` [('(', ')'), ('[', ']')]

-- | Detect whether user wrote something like @foo :: _@ or @foo :: (_, Int)@.
-- The former is considered toplevel case for which the function returns 'True',
-- the latter is not toplevel and the returned value is 'False'.
--
-- When type hole is at toplevel then there’s a line starting with
-- "• In the type signature" which ends with " :: _" like in the
-- following snippet:
--
-- source/library/Language/Haskell/Brittany/Internal.hs:131:13: error:
--     • Found type wildcard ‘_’ standing for ‘HsDecl GhcPs’
--       To use the inferred type, enable PartialTypeSignatures
--     • In the type signature: decl :: _
--       In an equation for ‘splitAnnots’:
--           splitAnnots m@HsModule {hsmodAnn, hsmodDecls}
--             = undefined
--             where
--                 ann :: SrcSpanAnnA
--                 decl :: _
--                 L ann decl = head hsmodDecls
--     • Relevant bindings include
--       [REDACTED]
--
-- When type hole is not at toplevel there’s a stack of where
-- the hole was located ending with "In the type signature":
--
-- source/library/Language/Haskell/Brittany/Internal.hs:130:20: error:
--     • Found type wildcard ‘_’ standing for ‘GhcPs’
--       To use the inferred type, enable PartialTypeSignatures
--     • In the first argument of ‘HsDecl’, namely ‘_’
--       In the type ‘HsDecl _’
--       In the type signature: decl :: HsDecl _
--     • Relevant bindings include
--       [REDACTED]
errorMessageRefersToToplevelHole :: T.Text -> Bool
errorMessageRefersToToplevelHole msg =
  not (T.null prefix) && " :: _" `T.isSuffixOf` T.takeWhile (/= '\n') rest
  where
    (prefix, rest) = T.breakOn "• In the type signature:" msg
