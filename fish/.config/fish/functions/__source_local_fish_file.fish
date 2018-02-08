function __source_local_fish_file -d "Attempt to find and source the local fish file"
    set lc_file "~/.config/fish"(hostname)".fish"
    if test -e $lc_file
        source $lc_file
    else
        echo "No local fish file"
    end
end
