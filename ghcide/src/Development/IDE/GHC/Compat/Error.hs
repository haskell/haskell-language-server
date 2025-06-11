{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Development.IDE.GHC.Compat.Error (
  -- * Top-level error types and lens for easy access
  MsgEnvelope(..),
  msgEnvelopeErrorL,
  GhcMessage(..),
  -- * Error messages for the typechecking and renamer phase
  TcRnMessage (..),
  TcRnMessageDetailed (..),
  stripTcRnMessageContext,
  -- * Parsing error message
  PsMessage(..),
  -- * Desugaring diagnostic
  DsMessage (..),
  -- * Driver error message
  DriverMessage (..),
  -- * General Diagnostics
  Diagnostic(..),
  -- * Prisms for error selection
  _TcRnMessage,
  _TcRnMessageWithCtx,
  _GhcPsMessage,
  _GhcDsMessage,
  _GhcDriverMessage,
  _TcRnMissingSignature,
  ) where

import           Control.Lens
import           GHC.Driver.Errors.Types
import           GHC.HsToCore.Errors.Types
import           GHC.Tc.Errors.Types
import           GHC.Types.Error

-- | Some 'TcRnMessage's are nested in other constructors for additional context.
-- For example, 'TcRnWithHsDocContext' and 'TcRnMessageWithInfo'.
-- However, in most occasions you don't need the additional context and you just want
-- the error message. @'_TcRnMessage'@ recursively unwraps these constructors,
-- until there are no more constructors with additional context.
--
-- Use @'_TcRnMessageWithCtx'@ if you need the additional context. You can always
-- strip it later using @'stripTcRnMessageContext'@.
--
_TcRnMessage :: Fold GhcMessage TcRnMessage
_TcRnMessage = _TcRnMessageWithCtx . to stripTcRnMessageContext

_TcRnMessageWithCtx :: Prism' GhcMessage TcRnMessage
_TcRnMessageWithCtx = prism' GhcTcRnMessage (\case
  GhcTcRnMessage tcRnMsg -> Just tcRnMsg
  _ -> Nothing)

_GhcPsMessage :: Prism' GhcMessage PsMessage
_GhcPsMessage = prism' GhcPsMessage (\case
  GhcPsMessage psMsg -> Just psMsg
  _ -> Nothing)

_GhcDsMessage :: Prism' GhcMessage DsMessage
_GhcDsMessage = prism' GhcDsMessage (\case
  GhcDsMessage dsMsg -> Just dsMsg
  _ -> Nothing)

_GhcDriverMessage :: Prism' GhcMessage DriverMessage
_GhcDriverMessage = prism' GhcDriverMessage (\case
  GhcDriverMessage driverMsg -> Just driverMsg
  _ -> Nothing)

-- | Some 'TcRnMessage's are nested in other constructors for additional context.
-- For example, 'TcRnWithHsDocContext' and 'TcRnMessageWithInfo'.
-- However, in some occasions you don't need the additional context and you just want
-- the error message. @'stripTcRnMessageContext'@ recursively unwraps these constructors,
-- until there are no more constructors with additional context.
--
stripTcRnMessageContext :: TcRnMessage -> TcRnMessage
stripTcRnMessageContext = \case
#if MIN_VERSION_ghc(9, 6, 1)
  TcRnWithHsDocContext _ tcMsg -> stripTcRnMessageContext tcMsg
#endif
  TcRnMessageWithInfo _ (TcRnMessageDetailed _ tcMsg) -> stripTcRnMessageContext tcMsg
  msg -> msg

msgEnvelopeErrorL :: Lens' (MsgEnvelope e) e
msgEnvelopeErrorL = lens errMsgDiagnostic (\envelope e -> envelope { errMsgDiagnostic = e } )

makePrisms ''TcRnMessage
