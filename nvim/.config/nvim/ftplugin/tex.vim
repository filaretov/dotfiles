" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
setlocal makeprg=latexmk\ -pdf\ -cd\ %
let b:ncm2_look_enabled = 1

setlocal expandtab
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2

" Depends on surround.vim
vmap <buffer> xb S}i\textbf<esc>
vmap <buffer> xi S}i\textit<esc>
