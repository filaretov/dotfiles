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
set -x PATH ~/go/bin $PATH
set -x PATH ~/media/packages/julia-1.4.1/bin/ $PATH
set -x PATH ~/.luarocks/bin/ $PATH
set -x PATH ~/.deno/bin/ $PATH
set -x PATH ~/.racket/bin/ $PATH
set -x PATH ~/.pyenv/bin/ $PATH
set -x PATH ~/.ghcup/bin/ $PATH
set -x PATH ~/.cabal/bin/ $PATH
set -x PATH ~/media/packages/node-v12.18.3-linux-x64/bin/ $PATH

set -x FZF_DEFAULT_COMMAND "rg --files"

alias ydl "~/.miniconda/envs/ydl/bin/youtube-dl"
eval (ssh-agent -c) >/dev/null; and ssh-add-all

source "$fish_path/conda.fish"
