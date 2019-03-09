#
# ~/.bashrc
#

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

PATH="/home/filaret/.local/bin/:$PATH"
export PATH
export VISUAL=nvim
export EDITOR="$VISUAL"
# python specific
export PYTHONDONTWRITEBYTECODE=1

# added by Miniconda3 installer
export PATH="/home/h.filaretov/.miniconda/bin:$PATH"
. /home/h.filaretov/.miniconda/etc/profile.d/conda.sh
# added by Miniconda3 4.5.12 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$(CONDA_REPORT_ERRORS=false '/home/hgf/.miniconda3/bin/conda' shell.bash hook 2> /dev/null)"
if [ $? -eq 0 ]; then
    \eval "$__conda_setup"
else
    if [ -f "/home/hgf/.miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/hgf/.miniconda3/etc/profile.d/conda.sh"
        CONDA_CHANGEPS1=false conda activate base
    else
        \export PATH="/home/hgf/.miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda init <<<
