GuiFont Fira Code:h10

nnoremap <expr> <silent> <C-z> (&buftype == 'terminal') ? ":bw!<cr>" : ":terminal<cr>"
nnoremap <expr> <silent> <C-tab> (&buftype == 'terminal') ? "bw!<cr>" : (bufexists("qterm")) ? "10<c-w>s:b qterm<cr>" : "10<c-w>s:te<cr><C-\><C-n>:f qterm<cr>"
tnoremap <silent> <C-tab> <C-\><C-n>:bw!<cr>
