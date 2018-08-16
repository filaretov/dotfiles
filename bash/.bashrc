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
