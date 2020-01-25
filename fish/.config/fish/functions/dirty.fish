function dirty
    set curr_pwd (pwd)
    cd ~/dev
    set dirs (fdfind --absolute-path --max-depth 4 --type --type d --hidden '.git$')

    for d in $dirs
        cd $d/..
        set git_status (git status --porcelain)
        if test -n "$git_status"
            if test "$argv[1]" = "cd"
                cd (string replace -a '.git' '' "$d")
                return 0
            end
            echo (string replace "$HOME" '' (string replace -a '/.git' '' "$d"))
        end
    end

    cd $curr_pwd
end
