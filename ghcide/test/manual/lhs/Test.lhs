-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


\subsection{Testing LHS}

\begin{code}
{-# LANGUAGE CPP #-}

module Test
    (
      main
    ) where


import Bird

\end{code}

for this file, \emph{hlint} should be turned off.
\begin{code}
{-# ANN module ("HLint: ignore" :: String) #-}
\end{code}

our main procedure

\begin{code}

main :: IO ()
main = do
  putStrLn "hello world."
  fly

\end{code}


