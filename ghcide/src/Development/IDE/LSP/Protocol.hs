-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs           #-}

module Development.IDE.LSP.Protocol
    ( pattern EventFileDiagnostics
    ) where

import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Language.LSP.Types

----------------------------------------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------------------------------------

-- | Pattern synonym to make it a bit more convenient to match on diagnostics
-- in things like damlc test.
pattern EventFileDiagnostics :: FilePath -> [Diagnostic] -> FromServerMessage
pattern EventFileDiagnostics fp diags <- FromServerMess STextDocumentPublishDiagnostics
    (NotificationMessage _ STextDocumentPublishDiagnostics
        (PublishDiagnosticsParams (uriToFilePath' -> Just fp) _ver (List diags)))
