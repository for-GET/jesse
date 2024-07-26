#!/usr/bin/env bash
set -euo pipefail

# NOTE sync with ./.github/workflows/ci.yml

if [ $(awk -F. '{print $1}' /etc/debian_version) -eq 9 ]; then
    printf 'deb http://archive.debian.org/debian/ stretch main contrib non-free deb http://archive.debian.org/debian-security/ stretch/updates main contrib non-free deb http://archive.debian.org/debian/ stretch-backports main contrib non-free' > /etc/apt/sources.list;
    apt-get -y update
    apt-get -y upgrade
fi

# manually sync with ./.github/workflows/ci.yml
apt-get -y update
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
