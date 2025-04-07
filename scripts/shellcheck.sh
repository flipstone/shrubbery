#!/bin/sh

set -o errexit
set -o xtrace

. scripts/lib/run-in-container.sh

apt update
apt install --yes file

# We want to ignore things in a handful of directories related to stack, cabal and git, but
# otherwise find shell scripts and run shellcheck against them.
find . -type f -exec file {} + | grep -v -e '.stack-work' -e 'stack-root' -e 'dist-newstyle' -e '.git' | awk -F: '/shell script/{print $1}' | xargs stack exec shellcheck -- -x
