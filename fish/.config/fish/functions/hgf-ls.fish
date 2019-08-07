# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
function hgf-ls -d "Custom ls replacement. Meant to be bind. See code for details"
    exa -lh
    commandline -f repaint
end
