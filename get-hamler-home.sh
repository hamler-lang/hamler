#!/bin/sh
set -e

# --- use sudo if we are not already root ---
SUDO=sudo
if [ $(id -u) -eq 0 ]; then
    SUDO=
fi

if $SUDO cabal --help >/dev/null 2>&1; then 
    if [ "$(uname -s)" = 'Darwin' ]; then
        HAMLER_HOME="/usr/local/lib/hamler"
    else
        HAMLER_HOME="/usr/lib/hamler"
    fi
else
    HAMLER_HOME="$HOME/.hamler"
fi

rc_file="$HOME/.$(basename $(echo $SHELL))rc"
echo "export PATH=$HAMLER_HOME:\$PATH" >> $rc_file
echo $HAMLER_HOME
