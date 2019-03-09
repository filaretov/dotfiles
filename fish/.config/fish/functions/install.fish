function install --wraps dnf -d "Package installation wrapper"
    set -l cmd "sudo dnf install"
    set -l log_file ~/Documents/Cloud/Log/installed
    echo $argv >> $log_file
    eval "$cmd $argv"
end
