#!/usr/bin/env bash

set -eu

BRANCH_NAME="${TRAVIS_BRANCH:=unknown}"

lein do clean, check, test :all, eastwood '{:source-paths ["src" "test"]}'

if [[ "${TRAVIS_BRANCH}" != "master" ]]; then
    exit 0
fi

ci/release.sh
