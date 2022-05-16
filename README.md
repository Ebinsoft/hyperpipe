# hyperpipe
![Build Status](https://github.com/Ebinsoft/hyperpipe/actions/workflows/haskell.yml/badge.svg)

`hyperpipe` is a many-to-many network tapping tool for linux systems.  It
leverages libpcap to collect raw traffic from one or more network interfaces
than can be routed out through other interfaces on the system.  Traffic can also
be marked with VLAN tags based on origin or destination.


## Installation

### Dependencies
Before compiling the source code, make sure you have the development libraries
for libpcap and ncurses installed on your system (not necessary if you are using
Nix).

On systems using *apt* (Debian, Ubuntu)
```bash
sudo apt install libpcap-dev libncurses-dev 
```

On systems using *yum* (RHEL, CentOS)
```bash
sudo yum install libpcap-devel ncurses-devel
```

### Stack
To build and install hyperpipe with [stack](https://www.haskellstack.org), run
the following commands:
```bash
git clone https://github.com/Ebinsoft/hyperpipe.git
cd hyperpipe
stack build
stack install
```

This will install two executables that are described in the sections below:
[`hyperpipe-daemon`](#hyperpipe-daemon), and [`hypertop`](#hypertop).

#### D-Bus
Hyperpipe uses the D-Bus system bus to share diagnostic information during
runtime, but probably won't have permission to use it by default. You'll need to
add a policy for Hyperpipe by copying the (config
file)[scripts/assets/hyperpipe.conf] from this repo into the appropriate dbus
folder:
```bash
sudo cp scripts/assets/hyperpipe.conf /etc/dbus-1/system.d/
```

## `hyperpipe-daemon`
This is the actual "hyperpipe" program that sniffs and injects traffic on the
network devices.  You can run it from the command line by simply giving it the
path to a valid configuration file.

You will also most likely need to run `hyperpipe-daemon` as root as it needs
access to raw network sockets and the dbus system bus.  If you want to run it as
another user, you will need to do two things. First, modify the `<policy
user="root">` line of the D-Bus config and replace `root` with your chosen user.
Then you will need to add the `CAP_NET_RAW` and `CAP_NET_ADMIN` capabilities to
the `hyperpipe-daemon` executable (see [linux
documentation](https://man7.org/linux/man-pages/man7/capabilities.7.html) for
details).

### Configuration files
Hyperpipe config files follow the YAML format, and specify which network
interfaces are to be used as inputs and outputs for the flow of traffic.
Hyperpipe will aggregate all of the traffic that arrives on the input
interfaces, and replicate that traffic onto each output.

For example, consider this config file for a system with four network devices
(`eth0` through `eth4`):
```yaml
inputs:
  # every packet received on eth0 will be given the VLAN tag 12
  eth0:
    vlan: 12
  # every packet received on eth2 will be given the VLAN tag 34
  eth1:
    vlan: 34

outputs:
  # note that an interface without any extra options still needs the ':'
  eth2:
  # setting the vlan option to 'null' will remove any existing VLAN tags from the packets
  eth3:
    vlan: null
```

Every packet that arrives on `eth0` is given the VLAN tag "12" and is put onto
the internal packet queue.  Likewise, every packet that arrives on `eth1` is
given the VLAN tag "34", and is also put onto the same internal packet queue.
On the opposite end, Hyperpipe is simultaneously sending *each* packet out
through both `eth2` and `eth3`, but the VLAN tags are removed before sending on
`eth3`. Aside from the VLAN tags, the streams of traffic sent over the two
outputs are identical.


## `hypertop`
This is a top-like program for monitoring the runtime behavior of hyperpipe.  It
requires no special permissions, but will fail if `hyperpipe-daemon` is not
running, or if the dbus initialization failed for some reason.
