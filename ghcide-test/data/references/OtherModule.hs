module OtherModule (symbolDefinedInOtherModule, symbolDefinedInOtherOtherModule) where

import OtherOtherModule

symbolDefinedInOtherModule = 1

symbolLocalToOtherModule = 2

someFxn x = x + symbolLocalToOtherModule
