# [[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

feh --bg-scale ~/.background
~/.local/bin/stowed/set_xkeyboard &

exec qtile
