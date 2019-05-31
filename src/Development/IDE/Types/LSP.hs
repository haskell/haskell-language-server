-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Types.LSP
    ( HoverText(..)
    , Event(..)
    , VirtualResource(..)
    , getHoverTextContent
    ) where

import Control.DeepSeq
import qualified Data.Text as T
import GHC.Generics

import Development.IDE.Types.Diagnostics

-- | Different types of content we can show on hover.
data HoverText
    = HoverDamlCode !T.Text
      -- ^ Highlighted DAML-Code
    | HoverMarkdown !T.Text
      -- ^ Markdown text.
    deriving Show

getHoverTextContent :: HoverText -> T.Text
getHoverTextContent = \case
    HoverDamlCode t -> t
    HoverMarkdown t -> t

-- | Virtual resources
data VirtualResource = VRScenario
    { vrScenarioFile :: !FilePath
    , vrScenarioName :: !T.Text
    } deriving (Eq, Ord, Read, Show, Generic)
    -- ^ VRScenario identifies a scenario in a given file.
    -- This virtual resource is associated with the HTML result of
    -- interpreting the corresponding scenario.

instance NFData VirtualResource

-- | Compiler service events
data Event
    = EventFileDiagnostics !FileDiagnostics
      -- ^ @EventFileDiagnostics fileDiagnostics@
      -- How many validations have we finished of how many total
      -- together with new file diagnostics for a given file.
    | EventVirtualResourceChanged !VirtualResource T.Text
      -- ^ @EventVirtualResourceChanged resource contents@ a virtual
      -- resource @resource@ changed to @contents
      -- NOTE(JM,MH): Keep the contents lazy as we rely on it in
      -- 'manageOpenVRs'.
    | EventFileValidation Int Int
      -- ^ @EventFileValidation finishedValidations totalValidations @
      -- How many validations have we finished of how many total.
    | EventFatalError !T.Text
      -- ^ @EventFatalError reason@: A fatal error occurred in the compiler and
      -- the compiler cannot continue.
      deriving Show
