# hyperpipe
![Build Status](https://github.com/Ebinsoft/hyperpipe/actions/workflows/haskell.yml/badge.svg)

`hyperpipe` is a many-to-many network tapping tool for linux systems.
It leverages libpcap to collect raw traffic from one or more network interfaces than can be routed out through other interfaces on the system.
Traffic can also be marked with VLAN tags based on origin or destination.


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

### Configuration files
Hyperpipe config files follow the YAML format, and specify which network interfaces are to be used as inputs and outputs for the flow of traffic.

For example, here is a config file that pulls packets in from the interfaces eth0 and eth1, and sends packets out on the interfaces eth2 and eth3.
```yaml
inputs:
  # every packet received on eth0 will be given the VLAN tag 0x1234
  eth0:
    vlan: 0x1234
  # every packet received on eth2 will be given the VLAN tag 0xABCD
  eth1:
    vlan: 0xABCD

outputs:
  # note that an interface without any extra options still needs the ':'
  eth2:
  # setting the vlan option to 'null' will remove any existing VLAN tags from the packets
  eth3:
    vlan: null
```
