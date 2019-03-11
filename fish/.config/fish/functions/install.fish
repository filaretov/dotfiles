function install --wraps dnf -d "Package installation wrapper"
    sudo dnf install $argv
end
