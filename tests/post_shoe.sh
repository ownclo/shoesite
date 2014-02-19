#!/bin/bash

set -e

curl -i -POST -H\
    'Accept: application/json'\
    -d "$(cat tests/sample.json)"\
    http://localhost:3000/shoes
