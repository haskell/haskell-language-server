-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


\subsection{Bird-style LHS}

> module Bird
>     (
>       fly
>     ) where



what birds are able to do:

> fly :: IO ()
> fly = putStrLn "birds fly."


