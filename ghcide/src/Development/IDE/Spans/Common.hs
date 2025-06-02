{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Development.IDE.Spans.Common (
  unqualIEWrapName
, safeTyThingId
, safeTyThingType
, SpanDoc(..)
, SpanDocUris(..)
, emptySpanDoc
, spanDocToMarkdown
, spanDocToMarkdownForTest
, DocMap
, TyThingMap
, srcSpanToMdLink
) where

import           Control.DeepSeq
import           Data.Bifunctor               (second)
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text                    as T
import           Development.IDE.GHC.Util
import qualified Documentation.Haddock.Parser as H
import qualified Documentation.Haddock.Types  as H
import           GHC
import           GHC.Generics
import           System.FilePath

import           Control.Lens
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans  ()
import qualified Language.LSP.Protocol.Lens   as JL
import           Language.LSP.Protocol.Types

type DocMap = NameEnv SpanDoc
type TyThingMap = NameEnv TyThing

-- | Shows IEWrappedName, without any modifier, qualifier or unique identifier.
unqualIEWrapName :: IEWrappedName GhcPs -> T.Text
unqualIEWrapName = printOutputable . rdrNameOcc . ieWrappedName

-- From haskell-ide-engine/src/Haskell/Ide/Engine/Support/HieExtras.hs
safeTyThingType :: TyThing -> Maybe Type
safeTyThingType thing
  | Just i <- safeTyThingId thing = Just (varType i)
safeTyThingType (ATyCon tycon)    = Just (tyConKind tycon)
safeTyThingType _                 = Nothing

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)                         = Just i
safeTyThingId (AConLike (RealDataCon dataCon)) = Just (dataConWrapId dataCon)
safeTyThingId _                                = Nothing

-- Possible documentation for an element in the code
data SpanDoc
  = SpanDocString [HsDocString] SpanDocUris
  | SpanDocText   [T.Text] SpanDocUris
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

data SpanDocUris =
  SpanDocUris
  { spanDocUriDoc :: Maybe T.Text -- ^ The haddock html page
  , spanDocUriSrc :: Maybe T.Text -- ^ The hyperlinked source html page
  } deriving stock (Eq, Show, Generic)
    deriving anyclass NFData

emptySpanDoc :: SpanDoc
emptySpanDoc = SpanDocText [] (SpanDocUris Nothing Nothing)

-- | Convert `SpanDoc` to Markdown format.
--
-- Return a list `Text` includes haddock, document uri and source code uri,
-- each item can be empty and must end with '\\n' if exist. This is to prevent
-- subsequent render problem caused by the missing newline.
--
-- Example:
--
-- For return value ["xxxx","yyyy"], if we concat the list with inserting
-- a separate line(note by "---\n"),
-- it will result "xxxx---\nyyyy" and can't be rendered as a normal doc.
-- Therefore we check every item in the value to make sure they all end with '\\n',
-- this makes "xxxx\n---\nyyy\n" and can be rendered correctly.
--
-- Notes:
--
-- To insert a new line in Markdown, we need two '\\n', like ("\\n\\n"), __or__ a section
-- symbol with one '\\n', like ("***\\n").
spanDocToMarkdown :: SpanDoc -> [T.Text]
spanDocToMarkdown = \case
    (SpanDocString docs uris) ->
        let doc = T.pack $ haddockToMarkdown $ H.toRegular $ H._doc $ H.parseParas Nothing $
                      renderHsDocStrings docs
        in  go [doc] uris
    (SpanDocText txt uris) -> go txt uris
  where
    go [] uris = render <$> spanDocUrisToMarkdown uris
    go txt uris = init txt <> [render (last txt)] <> (render <$> spanDocUrisToMarkdown uris)
    -- If the doc is not end with an '\n', we append it.
    render txt
      | T.null txt = txt
      | T.last txt == '\n' = txt
      | otherwise = txt <> T.pack "\n"

spanDocUrisToMarkdown :: SpanDocUris -> [T.Text]
spanDocUrisToMarkdown (SpanDocUris mdoc msrc) = catMaybes
  [ linkify "Documentation" <$> mdoc
  , linkify "Source" <$> msrc
  ]

-- | Generate a markdown link.
--
-- >>> linkify "Title" "uri"
-- "[Title](Uri)"
linkify :: T.Text -> T.Text -> T.Text
linkify title uri = "[" <> title <> "](" <> uri <> ")"

spanDocToMarkdownForTest :: String -> String
spanDocToMarkdownForTest
  = haddockToMarkdown . H.toRegular . H._doc . H.parseParas Nothing

-- Simple (and a bit hacky) conversion from Haddock markup to Markdown
haddockToMarkdown
  :: H.DocH String String -> String

haddockToMarkdown H.DocEmpty
  = ""
haddockToMarkdown (H.DocAppend d1 d2)
  = haddockToMarkdown d1 ++ " " ++ haddockToMarkdown d2
haddockToMarkdown (H.DocString s)
  = escapeBackticks s
haddockToMarkdown (H.DocParagraph p)
  = "\n\n" ++ haddockToMarkdown p
haddockToMarkdown (H.DocIdentifier i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocIdentifierUnchecked i)
  = "`" ++ i ++ "`"
#if MIN_VERSION_haddock_library(1,10,0)
haddockToMarkdown (H.DocModule (H.ModLink i Nothing))
  = "`" ++ escapeBackticks i ++ "`"
-- See https://github.com/haskell/haddock/pull/1315
-- Module references can be labeled in markdown style, e.g. [some label]("Some.Module")
-- However, we don't want to use the link markup here, since the module name would be covered
-- up by the label. Thus, we keep both the label and module name in the following style:
-- some label ( `Some.Module` )
haddockToMarkdown (H.DocModule (H.ModLink i (Just label)))
  = haddockToMarkdown label ++ " ( `" ++ escapeBackticks i ++ "` )"
#else
haddockToMarkdown (H.DocModule i)
  = "`" ++ escapeBackticks i ++ "`"
#endif
haddockToMarkdown (H.DocWarning w)
  = haddockToMarkdown w
haddockToMarkdown (H.DocEmphasis d)
  = "*" ++ haddockToMarkdown d ++ "*"
haddockToMarkdown (H.DocBold d)
  = "**" ++ haddockToMarkdown d ++ "**"
haddockToMarkdown (H.DocMonospaced d)
  = "`" ++ removeUnescapedBackticks (haddockToMarkdown d) ++ "`"
haddockToMarkdown (H.DocCodeBlock d)
  = "\n```haskell\n" ++ haddockToMarkdown d ++ "\n```\n"
haddockToMarkdown (H.DocExamples es)
  = "\n```haskell\n" ++ unlines (map exampleToMarkdown es) ++ "\n```\n"
  where
    exampleToMarkdown (H.Example expr result)
      = ">>> " ++ expr ++ "\n" ++ unlines result
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url Nothing))
  = "<" ++ url ++ ">"
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url (Just label)))
  = "[" ++ haddockToMarkdown label ++ "](" ++ url ++ ")"
haddockToMarkdown (H.DocPic (H.Picture url Nothing))
  = "![](" ++ url ++ ")"
haddockToMarkdown (H.DocPic (H.Picture url (Just label)))
  = "![" ++ label ++ "](" ++ url ++ ")"
haddockToMarkdown (H.DocAName aname)
  = "[" ++ escapeBackticks aname ++ "]:"
haddockToMarkdown (H.DocHeader (H.Header level title))
  = replicate level '#' ++ " " ++ haddockToMarkdown title

haddockToMarkdown (H.DocUnorderedList things)
  = '\n' : (unlines $ map (("+ " ++) . trimStart . splitForList . haddockToMarkdown) things)
haddockToMarkdown (H.DocOrderedList things) =
#if MIN_VERSION_haddock_library(1,11,0)
  '\n' : (unlines $ map ((\(num, str) -> show num ++ ". " ++ str) . second (trimStart . splitForList . haddockToMarkdown)) things)
#else
  '\n' : (unlines $ map (("1. " ++) . trimStart . splitForList . haddockToMarkdown) things)
#endif
haddockToMarkdown (H.DocDefList things)
  = '\n' : (unlines $ map (\(term, defn) -> "+ **" ++ haddockToMarkdown term ++ "**: " ++ haddockToMarkdown defn) things)

-- we cannot render math by default
haddockToMarkdown (H.DocMathInline _)
  = "*cannot render inline math formula*"
haddockToMarkdown (H.DocMathDisplay _)
  = "\n\n*cannot render display math formula*\n\n"

-- TODO: render tables
haddockToMarkdown (H.DocTable _t)
  = "\n\n*tables are not yet supported*\n\n"

-- things I don't really know how to handle
haddockToMarkdown (H.DocProperty _)
  = ""  -- don't really know what to do

escapeBackticks :: String -> String
escapeBackticks ""       = ""
escapeBackticks ('`':ss) = '\\':'`':escapeBackticks ss
escapeBackticks (s  :ss) = s:escapeBackticks ss

removeUnescapedBackticks :: String -> String
removeUnescapedBackticks = \case
  '\\' : '`' : ss -> '\\' : '`' : removeUnescapedBackticks ss
  '`' : ss        -> removeUnescapedBackticks ss
  ""              -> ""
  s : ss          -> s : removeUnescapedBackticks ss

splitForList :: String -> String
splitForList s
  = case lines s of
      []           -> ""
      (first:rest) -> unlines $ first : map (("  " ++) . trimStart) rest

-- | Generate a source link for the 'Location' according to VSCode's supported form:
-- https://github.com/microsoft/vscode/blob/b3ec8181fc49f5462b5128f38e0723ae85e295c2/src/vs/platform/opener/common/opener.ts#L151-L160
--
srcSpanToMdLink :: Location -> T.Text
srcSpanToMdLink location =
  let
    uri = location ^. JL.uri
    range = location ^. JL.range
    -- LSP 'Range' starts at '0', but link locations start at '1'.
    intText n = T.pack $ show (n + 1)
    srcRangeText =
      T.concat
        [ "L"
        , intText (range ^. JL.start . JL.line)
        , ","
        , intText (range ^. JL.start . JL.character)
        , "-L"
        , intText (range ^. JL.end . JL.line)
        , ","
        , intText (range ^. JL.end . JL.character)
        ]

    -- If the 'Location' is a 'FilePath', display it in shortened form.
    -- This avoids some redundancy and better readability for the user.
    title = case uriToFilePath uri of
      Just fp -> T.pack (takeFileName fp) <> ":" <> intText (range ^. JL.start . JL.line)
      Nothing -> getUri uri

    srcLink = getUri uri <> "#" <> srcRangeText
  in
   linkify title srcLink
