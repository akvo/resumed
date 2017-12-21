#!/usr/bin/env bash

set -eu

if [[ "${TRAVIS_BRANCH}" != "master" ]]; then
    exit 0
fi

RELEASE_VERSION="1.${TRAVIS_BUILD_NUMBER}.${TRAVIS_COMMIT}"

lein with-profile +set-version set-version $RELEASE_VERSION

#gpg --batch --passphrase ${CLOJARS_GPG_PASSWORD} --import devops.asc
#mvn deploy -s /app/maven-ci-settings.xml -Dgpg.passphrase=${CLOJARS_GPG_PASSWORD}
lein deploy releases