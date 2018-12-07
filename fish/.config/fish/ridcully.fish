set -gx TERMINAL gnome-terminal
if test -f $HOME/.miniconda3/etc/fish/conf.d/conda.fish
    source $HOME/.miniconda3/etc/fish/conf.d/conda.fish
end

alias install "sudo apt install"
alias search "apt search"
alias remove "apt remove"

add-path "$HOME/.cargo/bin"
add-path "$HOME/.local/bin"
