# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
set -gx TERMINAL terminator
if test -f $HOME/.conda/etc/fish/conf.d/conda.fish
    source $HOME/.conda/etc/fish/conf.d/conda.fish
end

set -g fish_greeting "Don't panic!"

set -x PATH ~/.cargo/bin $PATH
set -x PATH ~/.local/bin $PATH
set -x PATH ~/.local/bin/stowed $PATH
set -x PATH ~/dev/go/bin $PATH
set -x PATH ~/go/bin $PATH
set -x PATH ~/.luarocks/bin/ $PATH
set -x PATH ~/.deno/bin/ $PATH
set -x PATH ~/.racket/bin/ $PATH
set -x PATH ~/.pyenv/bin/ $PATH
set -x PATH ~/.ghcup/bin/ $PATH
set -x PATH ~/.cabal/bin/ $PATH
set -x PATH ~/.poetry/bin/ $PATH
set -x PATH ~/.local/node-v14.15.1-linux-x64/bin/ $PATH
set -x PATH ~/.local/neovim/bin $PATH
set -x PATH ~/.local/racket/bin $PATH
set -x PATH ~/.local/julia-1.6.0/bin $PATH
set -x PATH /usr/lib/dart/bin $PATH

set -x PATH ~/.local/bin/stowed $PATH
set -x FZF_DEFAULT_COMMAND "rg --files"

alias ydl "~/.miniconda/envs/ydl/bin/youtube-dl"
alias e nvim
alias code code-insiders
alias kj kajero
eval (ssh-agent -c) >/dev/null; and ssh-add-all

source "$fish_path/conda.fish"

source "$fish_path/themes/nord.fish"

if type -q shellmark
    shellmark --out fish plug | source
end

