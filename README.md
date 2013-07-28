My personal configuration files.

The current setup is Xubuntu 12.04+ with the default session with Xmonad subbed in as the window manager.

    sudo apt-get install xmonad suckless-tools zsh rxvt-unicode emacs vim pidgin git

    ./install.sh


    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo sh -c 'echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'

    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 94558F59
    sudo sh -c 'echo "deb http://repository.spotify.com stable non-free" >> /etc/apt/sources.list.d/spotify.list'

    sudo add-apt-repository ppa:upubuntu-com/chat

    sudo add-apt-repository ppa:keks9n/skypetab
