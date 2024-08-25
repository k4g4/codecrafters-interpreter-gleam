#!/bin/sh
#
# This script is used to run your program on CodeCrafters
#
# This runs after .codecrafters/compile.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure
set -o pipefail # Correct return code returned

# TODO: Use --no-print-progress once https://github.com/gleam-lang/gleam/issues/2299 is implemented
rm out 2>/dev/null
mkfifo out
cat out | grep -v Compiled | grep -v Running &
exec gleam run --module main -- "$@" >out