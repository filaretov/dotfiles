" SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
" Keymaps ---{{{
vnoremap <localleader># <ESC>'<O#if 0<ESC>'>o#endif<ESC>
nnoremap <localleader># /endif<cr>V%<ESC>'<dd'>dd
" }}}
"

" Settings ---{{{
augroup filetype_c
	autocmd!
	autocmd FileType c setlocal foldmethod=indent
augroup END
" }}}
