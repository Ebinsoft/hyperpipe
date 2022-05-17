#!/usr/bin/env bash
set -eux

## Add D-Bus policy to allow hyperpipe-daemon to reserve com.hyperpipe name

sudo cp assets/hyperpipe.conf /etc/dbus-1/system.d/
