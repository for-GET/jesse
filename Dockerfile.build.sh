#!/usr/bin/env bash
set -euo pipefail

# NOTE sync with ./.github/workflows/ci.yml
apt-get -y update
apt-get -y install curl git make
