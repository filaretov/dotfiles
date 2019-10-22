# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT

# CRUCIAL, DO NOT REMOVE
alias nano="vim"

# Obligatory ls options
alias ll="ls -lvh --group-directories-first"
alias lla="ls -lavh --group-directories-first"
alias la="ls -A"

# Prevents accidentally clobbering files
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# Changing directories and managing files
alias ..="cd .."
alias ...="cd ../../"

# Package managing made easier for me
function command_exists() {
	hash "$1" 2>/dev/null;
}

function search(){
	if command_exists dnf; then
		dnf search $@
	elif command_exists pacman; then
		pacman -Ss $@
	elif command_exists apt; then
		apt search @
	else
		echo "Unknown package manager"
	fi
}

function install(){
	if command_exists dnf; then
		sudo dnf install $@
	elif command_exists pacman; then
		sudo pacman \-S $@
	elif command_exists apt; then
		apt install @
	else
		echo "Unknown package manager"
	fi
}

function vim(){
	if command_exists nvim;
	then
		nvim "$@"
	else
		command vim "$@"
	fi
}

# Clone all git repos from a file
function clone_all(){
	while read p; do
		echo $p
	done < $@
}

# Get all submodule urls from a repo
function get_all_urls(){
	git submodule foreach --quiet 'git config --get remote.origin.url'
}

# TMUX aliases
alias tmat="tmux attach-session -t"
alias tmls="tmux ls"
alias tmns="tmux new -s"

# show progress of dd, run in a separate terminal
alias ddprog="sudo kill -USR1 $(pgrep ^dd)"

# cmake convenience
alias cb="rm -rf build/ && mkdir build && cd build"

function tub_mount(){
	sshfs filaret@sshgate.tu-berlin.de:/home/users/f/filaret/irb-ubuntu/ ~/Projects/tuberlin
}

function sudo_bamboozle(){
	([ "$((1 + RANDOM % 10))" == 10 ] && (curl  https://i.redd.it/beao34e0y57z.jpg --output bamboozle.jpg 1>&- 2>&- &))
	timestamp="$(date +%s)"
	file="${timestamp}.bamboozle"
	command sudo touch $file
	command sudo chattr +i $file
	command sudo $@
}

alias which_bamboozle="hecking_bamboozle"
function hecking_bamboozle() {
	echo /usr/bin/$@
}

function copy() {
	xclip -sel clip < $@
}

alias ssh-add-all='ls ~/.ssh/!(*.pub|config|known_hosts) | xargs ssh-add'
