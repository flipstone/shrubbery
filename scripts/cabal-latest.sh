#!/bin/sh

set -o errexit

. scripts/lib/run-in-container.sh

ghcup install ghc latest --set

cabal update
cabal test --flag ci
