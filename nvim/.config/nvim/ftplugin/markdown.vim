" SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
" vim: foldmethod=marker foldlevelstart=0:

"Syntax Options {{{
syn region markdownEqn matchgroup=markdownEqnDelimiter start="\$" end="\$" keepend contains=markdownLineStart
syn region markdownEqn matchgroup=markdownEqnDelimiter start="^\s*\$\$.*$" end="^\s*\$\$\ze\s*$" keepend 
hi def link markdownEqnDelimiter Comment
"}}}
