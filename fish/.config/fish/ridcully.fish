# SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
set -gx TERMINAL gnome-terminal
if test -f $HOME/.miniconda3/etc/fish/conf.d/conda.fish
    source $HOME/.miniconda3/etc/fish/conf.d/conda.fish
end

set -x PATH ~/.local/bin $PATH
set -x PATH ~/.local/bin/stowed $PATH
