set -g theme_color_scheme base16-light
set -x FZF_DEFAULT_COMMAND 'rg --files'
set  fish_greeting "Don't panic!"

alias install "sudo dnf install"
alias search "dnf search"
alias add_path "set fish_user_paths $fish_user_paths"
alias trunk "tree -C | less -r"
alias e "emacsclient -c"

# Find local fish file
set -l lc_file "$HOME/.config/fish/"(hostname)".fish"
if test -e $lc_file
    source $lc_file
else
    echo "No local fish file in "(pwd)" $lc_file"
end

# quick ls
bind \el 'hgf-ls'

# Because of funky emacs ansi-term behaviour
function fish_title
  true
end
