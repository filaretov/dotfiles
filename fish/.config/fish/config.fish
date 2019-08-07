# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
set  fish_greeting "Don't panic!"

alias search "dnf search"
alias remove "sudo dnf remove"
alias clk-in "echo i (date +\"%Y/%m/%d %H:%M\") Work:Fraunhofer IPK >> ~/.journal/ipk.time"
alias clk-out "echo o (date +\"%Y/%m/%d %H:%M\") >> ~/.journal/ipk.time"
alias calc "conda activate stats; and python -ic 'import numpy as np'; and conda deactivate"

# Find local fish file
set -l lc_file "$HOME/.config/fish/"(hostname)".fish"
if test -e $lc_file
    source $lc_file
else
    echo "No local fish file in "(pwd)" $lc_file"
end

# Because of funky emacs ansi-term behaviour
function fish_title
  true
end

