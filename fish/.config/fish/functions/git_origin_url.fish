function git_origin_url --description 'Echo current git repo name'
    set -l git_url = (git config --get remote.origin.url)
    if test $status -eq 0
        echo (basename "$git_url" ".git")
    end
end
