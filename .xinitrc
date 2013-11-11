#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/usr/X11R6/lib/X11/xinit/.Xresources
sysmodmap=/usr/X11R6/lib/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f $userresources ]; then
    xrdb -merge $userresources
fi

if [ -f $usermodmap ]; then
    xmodmap $usermodmap
fi


start-pulseaudio-x11 &

xset r rate 200 30 &  # keyboard rate
numlockx on &  # activate numlock at start

volnoti &  # popup notifications for volume controls
xbindkeys &  # see .xkeybindrc

nitrogen --restore &  # set wallpaper

exec xmonad
