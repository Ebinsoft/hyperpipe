#!/usr/bin/env bash
set -eux

### Installs the non-haskell dependencies for hyperpipe (development libraries
### for libpcap and ncurses).

if command -v apt &> /dev/null; then
    sudo apt install libpcap-dev libncurses-dev
elif command -v yum &> /dev/null; then
    sudo yum install libpcap-devel ncurses-devel
else
    echo "It doesn't look like you have apt or yum installed..."
fi


