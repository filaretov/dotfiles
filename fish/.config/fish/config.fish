# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT

set fish_path "$HOME/.config/fish"
set fish_greeting "Don't panic!"

alias install "sudo dnf install"
alias search "sudo dnf search"
alias remove "sudo apt remove"
alias clk-in "echo i (date +\"%Y/%m/%d %H:%M\") Work:Fraunhofer IPK >> ~/.journal/ipk.time"
alias clk-out "echo o (date +\"%Y/%m/%d %H:%M\") >> ~/.journal/ipk.time"
alias calc "conda activate stats; and python -ic 'import numpy as np'; and conda deactivate"
alias mnt "udisksctl mount -b"
alias umnt "udisksctl unmount -b"
alias c "conda"
alias ca "conda activate"
alias jl "jupyter lab"
alias ls "exa"
alias ll "exa -l --git"
alias la "exa -a"
alias lla "exa -la --git"
alias tree "exa --tree"

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

### Conda
source "$fish_path/conda.fish"
if type -f starship >/dev/null ^/dev/null
    starship init fish | source
    function __conda_add_prompt; end
end

source $fish_path/themes/nord.fish

function zoxide-add --on-event fish_prompt
    zoxide add
end

abbr -a zi "z -i"
abbr -a za "zoxide add"
abbr -a zq "zoxide query"
abbr -a zr "zoxide remove"
