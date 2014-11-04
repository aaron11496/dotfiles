# My Personal Linux Configuration #

To install or re-install dotfiles just run `install.sh`


## Debian Jessie with Xfce4 ##

### Disable system beep ###

    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf


### Use NTP to make sure clock automatically updates (especially for DST) ###

    sudo apt-get install ntp


### Keyboard mute button doesn't unmute in Xfce4 ###

Currently, `xfce4-volumed` sucks when working with `pulseaudio`. Fortunately, there's a fork called `xfce4-volumed-pulse` that works great:

    https://launchpad.net/xfce4-volumed-pulse

    ./configure
    make
    sudo make install

It will bind to volume up/down/mute keys, but make sure you didn't already set shortcuts to those keys with `xfce4-keyboard-settings`.


### Keyboard settings ###

Edit `/etc/default/keyboard` and use one of these:

    XKBOPTIONS="compose:menu,ctrl:nocaps"
    XKBOPTIONS="compose:prsc,ctrl:nocaps"


### Wireless 7260 on Lenovo T440s with kernel 3.14 ###

You may need to add non-free apt repos, but then do this:

    sudo apt-get install firmware-iwlwifi

To fix connecting to certain routers, disabling IPv6 is necessary. Add these lines to `/etc/sysctl.conf` then run `sysctl -p`:

    net.ipv6.conf.all.disable_ipv6 = 1
    net.ipv6.conf.default.disable_ipv6 = 1
    net.ipv6.conf.lo.disable_ipv6 = 1
    net.ipv6.conf.eth0.disable_ipv6 = 1
    net.ipv6.conf.wlan0.disable_ipv6 = 1


### PulseAudio automatic switch output device on plug-in ###

Edit `/etc/pulse/default.pa` and add this line to the bottom:

    load-module module-switch-on-connect


### Google Chrome ###

    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'


### DropBox (amd64) ###

    wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
    sudo dpkg -i dropbox_*_amd64.deb
