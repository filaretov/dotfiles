# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
function git_prompt -d "Echoes git prompt"
    set -l git_repo (git rev-parse --is-inside-work-tree ^/dev/null)
    if test "$git_repo" = "true"
        echo -n -s (git_origin_url) "|" (__fish_git_prompt "%s") " " (__fish_git_prompt_informative_status) " "
    end
end
