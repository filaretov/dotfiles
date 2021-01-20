" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
" Keymaps ---{{{
vnoremap <localleader># <ESC>'<O#if 0<ESC>'>o#endif<ESC>
nnoremap <localleader># /endif<cr>V%<ESC>'<dd'>dd
" }}}
"

set expandtab
set shiftwidth=4

" Settings ---{{{
augroup filetype_c
	autocmd!
	autocmd FileType c setlocal foldmethod=indent
augroup END
" }}}
