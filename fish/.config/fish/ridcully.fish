set -gx TERMINAL gnome-terminal
if test -f $HOME/.miniconda3/etc/fish/conf.d/conda.fish
    source $HOME/.miniconda3/etc/fish/conf.d/conda.fish
end

set -x PATH "~/.cargo/bin/" $PATH
set -x PATH "~/.poetry/bin/" $PATH
