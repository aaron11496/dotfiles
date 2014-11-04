#!/bin/bash

USB_DEVICE_NUM=`pactl list short sinks | grep 'alsa_output.usb' | head -n 1 | cut -d$'\t' -f1`

if [ $USB_DEVICE_NUM ]; then
    for STREAM_NUM in `pactl list short sink-inputs | cut -d$'\t' -f1`
    do
        pactl move-sink-input $STREAM_NUM $USB_DEVICE_NUM;
    done
fi
