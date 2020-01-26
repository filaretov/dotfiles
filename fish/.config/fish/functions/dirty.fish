function dirty
    set curr_pwd (pwd)
    cd ~/dev
    set dirs (fdfind --absolute-path --max-depth 3 --type --type d --hidden '.git$')

    for _d in $dirs
        set d (string replace '/.git' '' "$_d")
        cd $d
        if contains "fetch" $argv
            git fetch
        end
        if contains "pull" $argv
            git pull
        end
        set what (git status --porcelain=1)
        if test -n "$what"
            echo $what
            set git_dirty "*"
            return 0
        end
        set git_ab (git status --porcelain=1 --branch | grep -o '\[ahead.*\]\|\[behind.*\]')
        if test -n "$git_dirty" -o -n "$git_ab"
            echo $git_dirty $git_ab
            if contains "cd" $argv
                cd $d
                return 0
            end
            echo (string replace "$HOME" '' $d) $git_dirty $git_ab
        end
    end

    cd $curr_pwd
end
