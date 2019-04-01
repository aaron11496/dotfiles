# Aaron's Dotfiles #

To install or re-install dotfiles just run `install.sh`. It installs every
configuration file from this repo as a symlink from `~/`, creating
subdirectories as necessary.


## Debian Jessie with Xfce4 ##

### Automatic Monitor Configuration ###

Currently, xfce4 doesn't seem to have a way to automatically reconfigure
displays when a monitor is plugged or unplugged. Fortunately, there's a
tool called `autorandr` that can do that for us with a simple udev rule.

Install this fork of `autorandr`:
https://github.com/phillipberndt/autorandr

Use the `autorandr` instructions to create profiles for all of your
monitor setups.


### Use Slock instead of other lockers ###

    sudo apt-get install slock
    sudo mv /usr/bin/xflock4{,.bak}
    sudo ln -s /usr/bin/slock /usr/bin/xflock4


### Disable system beep ###

    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf


### Use NTP to make sure clock automatically updates (especially for DST) ###

    sudo apt-get install ntp


### Keyboard mute button doesn't unmute in Xfce4 ###

Currently, `xfce4-volumed` sucks when working with
`pulseaudio`. Fortunately, there's a fork called `xfce4-volumed-pulse`
that works great. Make sure you install the `libnotify-dev` package before
building or else you won't see pop-ups when changing the volume.

    https://launchpad.net/xfce4-volumed-pulse

    ./configure
    make
    sudo make install

It will bind to volume up/down/mute keys, but make sure you didn't already
set shortcuts to those keys with `xfce4-keyboard-settings`.


### Keyboard settings ###

Edit `/etc/default/keyboard` and use one of these:

    XKBOPTIONS="compose:menu,ctrl:nocaps"
    XKBOPTIONS="compose:prsc,ctrl:nocaps"


### Wireless 7260 on Lenovo T440s with kernel 3.14 ###

You may need to add non-free apt repos, but then do this:

    sudo apt-get install firmware-iwlwifi

To fix connecting to certain routers, disabling IPv6 is necessary. Add
these lines to `/etc/sysctl.conf` then run `sysctl -p`:

    net.ipv6.conf.all.disable_ipv6 = 1
    net.ipv6.conf.default.disable_ipv6 = 1
    net.ipv6.conf.lo.disable_ipv6 = 1
    net.ipv6.conf.eth0.disable_ipv6 = 1
    net.ipv6.conf.wlan0.disable_ipv6 = 1


### PulseAudio automatic switch output device on plug-in ###

Edit `/etc/pulse/default.pa` and add this line to the bottom:

    load-module module-switch-on-connect


### Fix PulseAudio volume jumping when increased by 1% ###

Edit `/etc/pulse/default.pa`, find the line loading `module-udev-detect`
and change it to this:

    load-module module-udev-detect ignore_dB=1


### Configuring Docker

To make docker use a different directory for the graph, just create a
symlink from `/var/lib/docker` to wherever you have space.
