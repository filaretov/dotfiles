# [[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

nm-applet &
nextcloud &
redshift-gtk &
compton -bfD 4 &

feh --bg-scale ~/.background &
~/.local/bin/stowed/set_xkeyboard &

exec i3
