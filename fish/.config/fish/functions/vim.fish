function vim -d "Start nvim if available, otherwise vim"
    if test -n (command -s nvim)
        nvim $argv
    else
        command vim $argv
    end
end
