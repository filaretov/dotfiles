set -gx TERMINAL gnome-terminal
if test -f $HOME/.miniconda3/etc/fish/conf.d/conda.fish
    source $HOME/.miniconda3/etc/fish/conf.d/conda.fish
end

set fish_user_paths $fish_user_paths "~/.cargo/bin/"
