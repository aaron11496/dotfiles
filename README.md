My personal configuration files. To install, just run

    ./install.sh


Debian:

    Disable beep:
    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf

    If keyboard mute button doesn't unmute in Xfce:
    run xfce4-settings-editor
    go to xfce4-mixer
    set active-card and sound-card to "PlaybackBuiltinAudioAnalogStereoPulseAudioMixer"
    change takes effect immediately

    Keyboard settings:
    /etc/default/keyboard
    XKBOPTIONS="compose:menu,ctrl:nocaps"

    Wireless doesn't work on Lenovo T440s with kernel 3.14:
    Download iwlwifi drivers
    cp iwlwifi-*.ucode /lib/firmware

Apt repos:

    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'
