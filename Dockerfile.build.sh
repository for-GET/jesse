#!/usr/bin/env bash
set -euo pipefail

# manually sync with ./.github/workflows/ci.yml
apt-get update
apt-get -y install wget
apt-get -y install libz-dev libssl-dev libcurl4-gnutls-dev libexpat1-dev gettext cmake gcc
cd /usr/src/
wget https://github.com/git/git/archive/v2.18.0.tar.gz -O git.tar.gz
tar -xf git.tar.gz
(
    cd git-*
    make prefix=/usr/local all
    make prefix=/usr/local install
)
rm git.tar.gz
rm -r git-*
