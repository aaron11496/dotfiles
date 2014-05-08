My personal configuration files.

    ./install.sh

Ubuntu:

    sudo apt-get install xmonad suckless-tools zsh rxvt-unicode emacs vim pidgin git



    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'

    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 94558F59
    sudo sh -c 'echo "deb http://repository.spotify.com stable non-free" >> /etc/apt/sources.list.d/spotify.list'

    sudo add-apt-repository ppa:upubuntu-com/chat

    sudo add-apt-repository ppa:keks9n/skypetab


Arch:

    https://github.com/davidbrazdil/volnoti

    localectl set-x11-keymap us pc105 'compose:menu,terminate:ctrl_alt_bksp,ctrl:nocaps'


Debian:

    echo "blacklist pcspkr" > /etc/modprobe.d/nobeep.conf


    keyboard mute button doesn't unmute:
    run xfce4-settings-editor
    go to xfce4-mixer
    set active-card and sound-card to "PlaybackBuiltinAudioAnalogStereoPulseAudioMixer"
    change takes effect immediately

    /etc/default/keyboard
    XKBOPTIONS="compose:menu,ctrl:nocaps"

