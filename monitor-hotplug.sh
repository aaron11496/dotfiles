#!/bin/sh
# /etc/udev/rules.d/70-monitor-hotplug.rules should look like this:
# SUBSYSTEM=="drm", ACTION=="change", RUN+="/path/to/this/file/monitor-hotplug.sh"

export XAUTHORITY=/home/aaron/.Xauthority
export DISPLAY=:0.0

xrandr_command="/usr/bin/xrandr"
sed_command="/bin/sed"

is_connected=`DISPLAY=:0 $xrandr_command | $sed_command -n '/DP2 connected/p'`

if [ -n "$is_connected" ]; then
    $xrandr_command --output DP2 --above eDP1 --auto
else
    $xrandr_command --auto
fi
