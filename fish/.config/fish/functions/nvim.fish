function nvim -d "Start nvim if not running, otherwise use nvr"
    if test -z "$NVIM_LISTEN_ADDRESS"
        command nvim $argv
    else
        nvr $argv
    end
end
