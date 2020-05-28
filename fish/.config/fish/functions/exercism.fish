function exercism
    switch $argv[1]
        case "download"
            set -l directory (command exercism $argv)
            set -l exercism_ws (exercism workspace)"/users"
            cd $directory
            switch $directory
            case "$exercism_ws/*/python/*"
                pytest
            case "$exercism_ws/*/c/*"
                make
            case '*'
                echo "New language! Exciting."
            end
        case '*'
            command exercism $argv
    end
end

