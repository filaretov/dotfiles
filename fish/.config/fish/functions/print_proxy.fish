# SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
function print_proxy -d "Print current proxy env vars"
    echo $all_proxy
    echo $https_proxy
    echo $http_proxy
    echo $ftp_proxy
    echo $ALL_PROXY
    echo $HTTPS_PROXY
    echo $HTTP_PROXY
    echo $FTP_PROXY
    echo $no_proxy
end
