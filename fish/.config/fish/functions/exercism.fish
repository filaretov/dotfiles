function exercism
    switch $argv[1]
        case "download"
            set -l directory (command exercism $argv ^/dev/null)
            cd $directory

            # this will only match when downloading students' submissions
            set -l exercism_ws (exercism workspace)"/users"
            switch $directory
            case "$exercism_ws/*/python/*"
                pytest
            case "$exercism_ws/*/c/*"
                make
            case '*'
            end
        case '*'
            command exercism $argv
    end
end

