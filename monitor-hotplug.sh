#!/bin/sh
# /etc/udev/rules.d/70-monitor-hotplug.rules should look like this:
# SUBSYSTEM=="drm", ACTION=="change", RUN+="/path/to/this/file/monitor-hotplug.sh"

export XAUTHORITY=/home/aaron/.Xauthority
export DISPLAY=:0.0

normal_display="eDP1"
xrandr_command="/usr/bin/xrandr"
sed_command="/bin/sed"

connected_display=`DISPLAY=:0 $xrandr_command | $sed_command -n 's/\b\(DP[12]\)\b connected.*/\1/p'`

if [ -n "$connected_display" ]; then
    $xrandr_command --output $connected_display --right-of $normal_display --auto
else
    $xrandr_command --auto
fi
