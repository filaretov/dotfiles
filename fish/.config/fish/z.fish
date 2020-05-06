function _z_cd
    cd $argv
    or return $status

    commandline -f repaint

    if test -n "$_ZO_ECHO"
        echo $PWD
    end
end

function z
    set argc (count $argv)

    if test $argc -eq 0
        _z_cd $HOME
        or return $status

    else if test $argc -eq 1 -a $argv[1] = '-'
        _z_cd -
        or return $status

    else
        # FIXME: use string-collect from fish 3.1.0 once it has wider adoption
        set -l IFS ''
        set -l result (zoxide query $argv)

        if test -d $result
            _z_cd $result
            or return $status
        else if test -n "$result"
            echo $result
        end
    end
end


abbr -a zi 'z -i'
abbr -a za 'zoxide add'
abbr -a zq 'zoxide query'
abbr -a zr 'zoxide remove'


function _zoxide_hook --on-event fish_prompt
    zoxide add
end

