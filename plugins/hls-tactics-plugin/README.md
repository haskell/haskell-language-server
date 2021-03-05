<p align="center">
<img src="https://haskellwingman.dev/wingman.png" height="256" alt="Wingman for Haskell" title="Wingman for Haskell">
</p>

<p>&nbsp;</p>

# Wingman for Haskell

[![Hackage](https://img.shields.io/hackage/v/hls-tactics-plugin.svg?logo=haskell&label=hls-tactics-plugin)](https://hackage.haskell.org/package/hls-tactics-plugin)

"Focus on the important stuff; delegate the rest"


## Dedication

> There's a lot of automation that can happen that isn't a replacement of
> humans, but of mind-numbing behavior.
>
> --Stewart Butterfield


## Overview

Wingman writes the boring, auxiliary code, so you don't have to. Generate
functions from type signatures, and intelligently complete holes.


## Getting Started

Wingman for Haskell is enabled by default in all [official release of Haskell
Language Server.][hls] Just hover over a typed hole, run the "Attempt to
fill hole" code action, *et voila!*

[hls]: https://github.com/haskell/haskell-language-server/releases


## Features

* [Type-directed code synthesis][auto], including pattern matching and recursion
* [Automatic case-splitting][case] --- just run the "Case split on <x>" code action
* [Smart next actions][next], for those times it can't read your mind

[auto]: https://haskellwingman.dev/foldr.gif
[case]: https://haskellwingman.dev/case-split.gif
[next]: https://haskellwingman.dev/intros.gif


## Support

Please consider [pledging on Patreon][patreon] to support the project and get
access to cutting-edge features.

[patreon]: https://www.patreon.com/wingman_for_haskell

