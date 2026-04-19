module Development.IDE.Plugin.Completions.Snippet where

import           Control.Lens
import           Data.String                              (IsString)
import           Data.Text                                (Text)
import           Development.IDE.Plugin.Completions.Types
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Types

data SnippetCompletion = SnippetCompletion
  { snippetLabel    :: {-# UNPACK #-} !Text,
    snippetDetail   :: {-# UNPACK #-} !Text,
    -- | Might be good to use the structured snippets instead of bare text.
    -- This is fine for now though, none of the top-level snippet completions are
    -- parameterized.
    snippetContents :: {-# UNPACK #-} !Text
  }

topContextSnippets :: [SnippetCompletion]
topContextSnippets =
  [ SnippetCompletion "import" "import module" importUnqualifiedSnippet,
    SnippetCompletion "import" "import module (explicit list)" importExplicitSnippet,
    SnippetCompletion "import" "import module hiding" importHidingSnippet,
    SnippetCompletion "import" "import module qualified as" importQualifiedAsSnippet,
    SnippetCompletion "function" "function definition" functionDefinitionSnippet,
    SnippetCompletion "class" "class declaration" classDeclarationSnippet,
    SnippetCompletion "instance" "instance declaration" instanceDeclarationSnippet
  ]

mkTopSnippetCompl :: SnippetCompletion -> CompletionItem
mkTopSnippetCompl SnippetCompletion {..} =
  defaultCompletionItemWithLabel snippetLabel
    & L.kind ?~ CompletionItemKind_Snippet
    & L.detail ?~ snippetDetail
    & L.insertText ?~ snippetContents
    & L.insertTextFormat ?~ InsertTextFormat_Snippet

importUnqualifiedSnippet :: (IsString s) => s
importUnqualifiedSnippet = "import ${1:module}"

importExplicitSnippet :: (IsString s) => s
importExplicitSnippet = "import ${1:module} (${2:names})"

importHidingSnippet :: (IsString s) => s
importHidingSnippet = "import ${1:module} hiding (${2:names})"

importQualifiedAsSnippet :: (IsString s) => s
importQualifiedAsSnippet = "import ${1:module} qualified as ${2:alias}"

functionDefinitionSnippet :: (IsString s) => s
functionDefinitionSnippet =
  "${1:identifier} :: ${2:type}\n\
  \${1:identifier} = ${3:body}"

classDeclarationSnippet :: (IsString s) => s
classDeclarationSnippet = "class ${1:name} where"

instanceDeclarationSnippet :: (IsString s) => s
instanceDeclarationSnippet = "instance ${1:name} where"
