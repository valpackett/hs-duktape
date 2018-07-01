#!/bin/bash

mkdir -p dukconfig/dist

python duktape/tools/configure.py \
       --output-directory dukconfig/dist \
       --source-directory duktape/src-input \
       --config-metadata duktape/config \
       --option-file dukconfig/config.yaml \
