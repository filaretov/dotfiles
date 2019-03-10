#!/bin/bash
# xsel didn't work for some reason
pwgen 64 1 | xclip -sel clip
notify-send "Password copied to clipboard" -t 750
