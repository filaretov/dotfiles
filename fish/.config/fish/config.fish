# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT

set fish_path "$HOME/.config/fish"
set fish_greeting ""

alias install "sudo apt install"
alias search "sudo apt search"
alias remove "sudo apt remove"
alias mnt "udisksctl mount -b"
alias umnt "udisksctl unmount -b"
alias ca "conda activate"
alias jl "jupyter lab"
alias ls "exa"
alias ll "exa --git-ignore -l --git --group-directories-first"
alias la "exa -a"
alias lla "exa -la --git"
alias tree "exa --tree"
alias n "nvr -s"
alias clip "xclip -sel clip"
alias kaj kajero
alias q quest
alias k khal
alias t todo
alias por "poetry run"
alias pos "poetry shell"
alias dk "diskonaut"

# Find local fish file
set -l lc_file "$fish_path/"(hostname)".fish"
if test -e $lc_file
    source $lc_file
else
    echo "No local fish file in $lc_file"
end

# Because of funky emacs ansi-term behaviour
function fish_title
    true
end

if type -f starship >/dev/null ^/dev/null
    starship init fish | source
    function __conda_add_prompt; end
end

source $fish_path/themes/nord.fish

set -x EDITOR "nvim"

if test -e "$fish_path/z.fish"
    source "$fish_path/z.fish"
end

status --is-interactive; and source (pyenv init -|psub)
