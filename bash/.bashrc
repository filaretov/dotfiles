# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source global definitions
if [ -a /etc/bashrc ]; then
	. /etc/bashrc
fi

#User specific aliases and functions
if [[ -a ~/.bash_aliases ]]
then
	source ~/.bash_aliases
fi

# Device specific bash settings
if [[ -a ~/.bash_extra ]]
then
	source ~/.bash_extra
fi

#User specific proxy settings
if [ -f ~/.proxy.bash ]; then
	. ~/.proxy.bash
fi

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export VISUAL=nvim
export EDITOR="$VISUAL"
# python specific
export PYTHONDONTWRITEBYTECODE=1

shopt -s extglob

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/hgf/.miniconda/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/hgf/.miniconda/etc/profile.d/conda.sh" ]; then
        . "/home/hgf/.miniconda/etc/profile.d/conda.sh"
    else
        export PATH="/home/hgf/.miniconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.local/bin/stowed:$PATH"
