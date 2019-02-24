function install --wraps dnf -d "Package manager wrapper"
    set -l cmd "sudo dnf install"
    if string match -q -- "*Ubuntu*" (uname -a)
        set cmd "sudo apt install"
    end
    set -l log_file ~/Documents/cloud/Log/installed
    echo $argv >> $log_file
    eval "$cmd $argv"
end
