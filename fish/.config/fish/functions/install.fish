function install --wraps dnf -d "Package manager wrapper"
    set -l log_file ~/Documents/cloud/Log/installed
    echo $argv >> $log_file
    sudo dnf install $log_file
end
