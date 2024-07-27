#!/usr/bin/env bash
# Generate $DEPTH layers of modules with $WIDTH modules on each layer
# Every module on layer N imports all the modules on layer N-1
# MultiLayerModules.hs imports all the modules from the last layer
DEPTH=15
WIDTH=40
cat >hie.yaml << EOF
cradle:
  direct:
    arguments:
EOF
for i in $(seq -w 1 $WIDTH); do
  echo "module DummyLevel0M$i where" > DummyLevel0M$i.hs;
  echo "          - DummyLevel0M$i.hs" >> hie.yaml;
done
for l in $(seq 1 $DEPTH); do
  for i in $(seq -w 1 $WIDTH); do
    echo "module DummyLevel${l}M$i where" > DummyLevel${l}M$i.hs;
    echo "          - DummyLevel${l}M$i.hs" >> hie.yaml;
    for j in $(seq -w 1 $WIDTH); do
      echo "import DummyLevel$((l-1))M$j" >> DummyLevel${l}M$i.hs;
    done
  done
done
case "$1" in
  '--th')
    echo "{-# LANGUAGE TemplateHaskell #-}" > MultiLayerModules.hs
    ;;
esac
echo "module MultiLayerModules where" >> MultiLayerModules.hs
  echo "          - MultiLayerModules.hs" >> hie.yaml;
for j in $(seq -w 1 $WIDTH); do
  echo "import DummyLevel${DEPTH}M$j" >> MultiLayerModules.hs;
done
