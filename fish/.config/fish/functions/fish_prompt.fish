set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_dirtystate FCBC47
set __fish_git_prompt_color_stagedstate green
set __fish_git_prompt_color_upstream cyan

# Git Characters
set __fish_git_prompt_char_dirtystate '*'
set __fish_git_prompt_char_stagedstate '⇢'
set __fish_git_prompt_char_upstream_prefix ' '
set __fish_git_prompt_char_upstream_equal ''
set __fish_git_prompt_char_upstream_ahead '⇡'
set __fish_git_prompt_char_upstream_behind '⇣'
set __fish_git_prompt_char_upstream_diverged '⇡⇣'

function fish_prompt --description 'Write out the prompt'
    set -l last_status $status
    set -l normal (set_color normal)

    set -l color_cwd
    set -l prefix
    set -l suffix
    switch $USER
        case root toor
            if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
            else
                set color_cwd $fish_color_cwd
            end
            set suffix '#'
        case '*'
            set color_cwd $fish_color_cwd
            set suffix 'λ'
    end

    # Git prompt settings
    set -l git_repo (git rev-parse --is-inside-work-tree ^/dev/null)
    # git repo exists and EMACS string is zero-length
    if test "$git_repo" = "true" -a -z "$EMACS"
        echo -n -s (set_color blue) (basename (git rev-parse --show-toplevel)) $normal
        echo -n -s '->' (__fish_git_prompt "%s")
        # echo -n -s (set_color green) (__fish_git_prompt_informative_status) $normal " "
    end
    # PWD
    echo -n -s "(" (prompt_pwd) ")" " "
    # Suffix
    echo -n -s (set_color blue) $suffix $normal " "
end
