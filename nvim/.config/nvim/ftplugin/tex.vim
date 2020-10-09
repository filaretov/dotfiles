" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
setlocal makeprg=latexmk\ -pdf\ -cd\ %
let b:ncm2_look_enabled = 1

setlocal expandtab
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2

inoremap <buffer> ,g \gls{}<Left>
