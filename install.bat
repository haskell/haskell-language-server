:: Copyright (c) 2019 The DAML Authors. All rights reserved.
:: SPDX-License-Identifier: Apache-2.0

@REM Install hie-core where cabal install would put it on Windows
@REM but avoid checking configure or installing local libraries (faster)
ghc Main -o dist\obj\hie-core.exe -XBangPatterns -XDeriveGeneric -XGeneralizedNewtypeDeriving -XLambdaCase -XNamedFieldPuns -XRecordWildCards -XScopedTypeVariables -XStandaloneDeriving -XTupleSections -XTypeApplications -XViewPatterns -package=ghc -DGHC_STABLE -isrc -iexe -outputdir dist\obj && copy dist\obj\hie-core.exe %AppData%\cabal\bin\hie-core.exe
