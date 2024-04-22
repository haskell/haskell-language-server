HRK:|OK|FAIL|error

OK|FAIL|error|isEvaluating|queueForEvaluation|unqueueForEvaluation|writeBinCoreFile


(HRK: writeBinCoreFile)|(HRK: linkableType=Just BCOLinkable)|OK|FAIL|error

Theory:

Every test that succeeds has the following sequence:


HRK: linkableType=Just BCOLinkable
HRK: writeCoreFileIfNeeded: guts=modguts
HRK: writeBinCoreFile /tmp/hls-test-root/.cache/ghcide/test-0.1.0.0-inplace-1af041e620a747f3eee9125b6968276b7bf496d0/extra-file-30552027971537-267991-12








In order for `GetLinkable` rule to succeed, the

The core file is written [here](https://github.com/haskell/haskell-language-server/blob/9593d04a76e024942981b1333bfb2558a6ae0dab/ghcide/src/Development/IDE/Core/Compile.hs#L532)

But this core-file-writing code path is only triggered when  is called with `linkableType == Just BCOLinkable`.

In the case when we get the `GetLinkable` error, the `linkableType` is always `Nothing`.



