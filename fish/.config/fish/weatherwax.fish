# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
set -gx TERMINAL terminator
if test -f $HOME/.miniconda3/etc/fish/conf.d/conda.fish
    source $HOME/.miniconda3/etc/fish/conf.d/conda.fish
end

set -x PATH ~/.cargo/bin $PATH
set -x PATH ~/.local/bin $PATH
set -x PATH ~/.local/bin/stowed $PATH
set -x PATH ~/dev/go/bin $PATH
set -x PATH ~/media/packages/julia-1.4.1/bin/ $PATH
