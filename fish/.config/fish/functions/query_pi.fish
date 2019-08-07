# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
function query_pi -d "Query RPi at standard address whether it's online"
    set local_date (date)
    set pi_output (ssh pi "up" -o ConnectTimeout=20)
    if test $status -eq 0
        echo "$pi_output: $local_date" | tee -a ~/query_pi
        return 0
    else
        return 1
    end
end

