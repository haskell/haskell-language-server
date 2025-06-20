-- Support for property checking
module TProperty where

-- prop> \(l::[Bool]) -> head l
-- *** Failed! (after 1 test):
-- Exception:
--   Prelude.head: empty list
--   CallStack (from HasCallStack):
--     error, called at libraries/ghc-internal/src/GHC/Internal/List.hs:2030:3 in ghc-internal:GHC.Internal.List
--     errorEmptyList, called at libraries/ghc-internal/src/GHC/Internal/List.hs:96:11 in ghc-internal:GHC.Internal.List
--     badHead, called at libraries/ghc-internal/src/GHC/Internal/List.hs:90:28 in ghc-internal:GHC.Internal.List
--     head, called at <interactive>:1:27 in interactive:Ghci2
--   HasCallStack backtrace:
--     collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:92:13 in ghc-internal:GHC.Internal.Exception
--     toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:128:3 in ghc-internal:GHC.Internal.Exception
--   
-- []
