#!/bin/bash

nm-applet &
nextcloud &
redshift-gtk &
blueman-applet &
compton --daemon \
	--fading \
	--fade-delta=3 \
	&

feh --bg-scale ~/.background &
~/.local/bin/stowed/set_xkeyboard &
