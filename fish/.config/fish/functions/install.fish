# SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
function install --wraps dnf -d "Package installation wrapper"
    sudo dnf install $argv
end
