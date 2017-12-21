#!/usr/bin/env bash

set -e

BRANCH_NAME="${TRAVIS_BRANCH:=unknown}"

if [ -z "$TRAVIS_COMMIT" ]; then
    export TRAVIS_COMMIT=local
fi

if [[ "${TRAVIS_PULL_REQUEST}" != "false" ]]; then
    exit 0
fi

lein do clean, check, test :all, eastwood '{:source-paths ["src" "test"]}'

if [[ "${TRAVIS_BRANCH}" != "master" ]]; then
    exit 0
fi

ci/release.sh