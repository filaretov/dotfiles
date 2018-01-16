" Keymaps ---{{{
" Comment line
nnoremap <localleader>c I//<esc>
" Uncomment line
nnoremap <localleader>uc I<del><del><esc>
"Wrap () block in quotes from current word backwards
nnoremap <localleader>(" viw<Esc>a"<Esc>vabo<Esc>a"<Esc>t"
" }}}

" Settings ---{{{
augroup filetype_c
	autocmd!
	autocmd FileType c setlocal foldmethod=syntax
augroup END
" }}}
