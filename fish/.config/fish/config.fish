set -g theme_color_scheme base16-light
set -x FZF_DEFAULT_COMMAND 'rg --files'

alias install "sudo dnf install"
alias search "dnf search"
alias gu "git push"
alias gd "git fetch"
alias gcm "git commit -m"
alias gacm "git add .; and git commit -m"
alias v "nvim"
alias add_path "set fish_user_paths $fish_user_paths"

set -x PATH $PATH ~/.local/bin
set -x PATH $PATH ~/.arm/bin

# Find local fish file
set lc_file "~/.config/fish"(hostname)".fish"
if test -e $lc_file
    source $lc_file
else
    echo "No local fish file"
end
