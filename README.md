My personal configuration files. To install, just run

    ./install.sh


Debian:

    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf

    keyboard mute button doesn't unmute:
    run xfce4-settings-editor
    go to xfce4-mixer
    set active-card and sound-card to "PlaybackBuiltinAudioAnalogStereoPulseAudioMixer"
    change takes effect immediately

    /etc/default/keyboard
    XKBOPTIONS="compose:menu,ctrl:nocaps"


Arch:

    https://github.com/davidbrazdil/volnoti

    localectl set-x11-keymap us pc105 'compose:menu,terminate:ctrl_alt_bksp,ctrl:nocaps'
