-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.LSP.Protocol
    ( pattern EventFileDiagnostics
    ) where

import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types

----------------------------------------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------------------------------------

-- | Pattern synonym to make it a bit more convenient to match on diagnostics
-- in things like damlc test.
pattern EventFileDiagnostics :: FilePath -> [Diagnostic] -> FromServerMessage
pattern EventFileDiagnostics fp diags <-
    NotPublishDiagnostics
        (NotificationMessage _ _ (PublishDiagnosticsParams (uriToFilePath' -> Just fp) (List diags)))
