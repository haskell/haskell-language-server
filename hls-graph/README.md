# hls-graph - a limited reimplementation of Shake for in-memory build graphs

`ghcide` was originally built on top of [Shake](http://shakebuild.com), a Haskell build system. Nowadays Shake has been replaced by a special purpose implementation of a build graph called hls-graph, which drops all the persistency features in exchange for simplicity and performance.

Features:

* Dynamic dependencies
* User defined rules (there are no predefined File rules as in Shake)
* Build reports (a la Shake profiling)
* "Reactive" change tracking for minimal rebuilds (not available in Shake)

What's missing:

* Persistence
* A default set of rules for file system builds
* A testsuite
* General purpose application - many design decisions make assumptions specific to ghcide
