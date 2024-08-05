#!/bin/bash

# recommended to build test binary separately and then run it in a loop (to avoid running cabal test in a loop)
# Run tests in a loop
for i in {1..500}; do
    echo "Iteration $i" &&
    HLS_TEST_LOG_STDERR=1 TASTY_PATTERN="simple-multi-def-test" cabal test ghcide-tests \
    || {
        echo "Warning: error at iteration $i"
        break
        }; done
