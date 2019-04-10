# Aaron's Dotfiles #

To install or re-install dotfiles just run `install.sh`. It installs every
configuration file from this repo as a symlink from `~/`, creating
subdirectories as necessary.


## Debian Stretch with Xfce4 ##

### Packages ###

    sudo apt-get install accountsservice arandr blueman build-essential ca-certificates coreutils curl feh fonts-droid-fallback fonts-inconsolata fonts-ubuntu-title ntp pulseaudio-module-bluetooth python-pip redshift-gtk rsync rxvt-unicode silversearcher-ag software-properties-common suckless-tools tree ttf-dejavu virtualenvwrapper wireless-tools wpasupplicant xbindkeys xmonad zsh

Slack https://slack.com/downloads/instructions/ubuntu
Google Chrome https://www.google.com/chrome/
Docker CE https://docs.docker.com/install/linux/docker-ce/debian/
Spotify https://www.spotify.com/us/download/linux/
Autorandr https://packages.debian.org/sid/x11/autorandr


### Use Slock instead of other lockers ###

    sudo apt-get install slock
    sudo mv /usr/bin/xflock4{,.bak}
    sudo ln -s /usr/bin/slock /usr/bin/xflock4


### Disable system beep ###

    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf


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


### Configuring Docker

To make docker use a different directory for the graph, just create a
symlink from `/var/lib/docker` to wherever you have space.
