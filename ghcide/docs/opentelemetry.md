# Using opentelemetry

`ghcide` has support for opentelemetry-based tracing. This allows for tracing
the execution of the process, seeing when Shake rules fire and for how long they
run, when LSP messages are received, and (currently WIP) measuring the memory
occupancy of different objects in memory.

## Capture opentlemetry data

Capturing of opentelemetry data can be enabled by first building ghcide with eventlog support:

```sh
stack build --ghc-options -eventlog
```

Then, you can run `ghcide`, giving it a file to dump eventlog information into.

```sh
ghcide +RTS -l -ol ghcide.eventlog -RTS
```

You can also optionally enable reporting detailed memory data with `--ot-memory-profiling`

```sh
ghcide --ot-memory-profiling +RTS -A4G -l -ol ghcide.eventlog -RTS
```

*Note:* This option, while functional, is extremely slow. You will notice this because the memory graph in the output will have datapoints spaced apart by a couple of minutes. The nursery must be big enough (-A1G or larger) or the measurements will self-abort.

## Viewing with tracy

After installing `opentelemetry-extra` and `tracy`, you can view the opentelementry output:

```sh
eventlog-to-tracy ghcide.eventlog
```

If everything has been set up correctly, this should open a tracy window with the tracing data you captured

### Installing opentelemetry-extra

This package includes a number of binaries for converting between the eventlog output and the formats that various opentelemetry viewers (like tracy) can display:

```sh
cabal install openetelemetry-extra
```



### Building tracy

1. Install the dependencies: `pkg-config` and `glfw, freetype, capstone, GTK3`, along
   with their header files (`<pkgname>-dev` on most distros. On Arch the header
   files are included with the normal packages).
2. Download tracy from https://github.com/wolfpld/tracy
3. `cd` into the directory containing the source you downloaded
4. Build the `import-chrome` and `Tracy` libraries:
   ```sh
   make -C profiler/build/unix release
   make -C import-chrome/build/unix release
   ```
5. Copy the binaries to your `$PATH`:
   ```sh
   cp profiler/build/unix/Tracy-release ~/.local/bin/Tracy
   cp import-chrome/build/unix/import-chrome-release ~/.local/bin/import-chrome
   ```
