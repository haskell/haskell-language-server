#!/bin/bash

set -eux

for bindist in out-*.tar ; do
	tar xf "${bindist}"
done
