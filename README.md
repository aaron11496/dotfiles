My personal configuration files. To install, just run

    ./install.sh

Debian (Jessie with XFCE):

Disable beep:
    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf

Keyboard mute button doesn't unmute in Xfce4:
xfce4-volumed kinda sucks when working with pulseaudio. Fortunately, there's a fork called xfce4-volumed-pulse that works:

    https://launchpad.net/xfce4-volumed-pulse

    ./configure && make && make install

It will bind to volume up/down/mute, but make sure you didn't already set shortcuts to those keys with xfce4-keyboard-settings.


For keyboard settings, edit `/etc/default/keyboard` and use one of these:

    XKBOPTIONS="compose:menu,ctrl:nocaps"
    XKBOPTIONS="compose:prsc,ctrl:nocaps"


Wireless 7260 doesn't work on Lenovo T440s with kernel 3.14 (you may need to add non-free repos):

    sudo apt-get install firmware-iwlwifi


Installing apps not in repos:

Google Chrome:

    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list'


<DropBox (amd64)

    wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
    sudo dpkg -i dropbox_*_amd64.deb
