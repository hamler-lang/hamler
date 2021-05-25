#!/bin/sh
set -e
# --- use sudo if we are not already root ---
SUDO=sudo
if [ $(id -u) -eq 0 ]; then
    SUDO=
fi

if $SUDO cabal --help >/dev/null 2>&1; then 
    if [ "$(uname -s)" = 'Darwin' ]; then
        echo "/usr/local/lib/hamler"
    else
        echo "/usr/lib/hamler"
    fi
else
    echo "$HOME/.hamler"
fi
