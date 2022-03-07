# hyperpipe
![Build Status](https://github.com/Ebinsoft/hyperpipe/actions/workflows/haskell.yml/badge.svg)

`hyperpipe` is a many-to-many network tapping tool for linux systems.
It leverages libpcap to collect raw traffic from one or more network interfaces than can be routed out through other interfaces on the system.


## Installation
You can easily build and install hyperpipe using [stack](https://www.haskellstack.org)
```bash
git clone https://github.com/Ebinsoft/hyperpipe.git
cd hyperpipe
stack build
stack install
```

This repo also includes a derivation for the Nix package manager if you are using that.

## Usage
TODO
