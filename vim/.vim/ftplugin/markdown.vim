" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
" Basic Options {{{
set number
"}}}

"Syntax Options {{{
syn region markdownEqn matchgroup=markdownEqnDelimiter start="\$" end="\$" keepend contains=markdownLineStart
syn region markdownEqn matchgroup=markdownEqnDelimiter start="^\s*\$\$.*$" end="^\s*\$\$\ze\s*$" keepend 
hi def link markdownEqnDelimiter Comment
"}}}

"Remaps:{{{
nnoremap <localleader>c :w<cr>:Silent emdu pdf path 2<cr>
"}}}

"Abbreviations:{{{
iabbrev ,v \vec{}<Left>
"}}}
