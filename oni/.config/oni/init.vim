" vim: foldmethod=marker foldlevelstart=0:

set background=dark

" Plugins {{{

command! PackUpdate packadd minpac | source $MYVIMRC | redraw | call minpac#update()
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()

if exists('*minpac#init')
  call minpac#init()
  call minpac#add('justinmk/vim-dirvish')
  call minpac#add('kballard/vim-fish')
  call minpac#add('k-takata/minpac', {'type': 'opt'})
  call minpac#add('lervag/vimtex')
  call minpac#add('romainl/flattened')
  call minpac#add('rust-lang/rust.vim')
  call minpac#add('sgur/vim-editorconfig')
  call minpac#add('Shougo/neosnippet.vim')
  call minpac#add('tommcdo/vim-exchange')
  call minpac#add('tpope/vim-abolish')
  call minpac#add('tpope/vim-commentary')
  call minpac#add('tpope/vim-eunuch')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('tpope/vim-obsession')
  call minpac#add('tpope/vim-repeat')
  call minpac#add('tpope/vim-rsi')
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')
  call minpac#add('filaretov/vim-todo')
  call minpac#add('filaretov/vim-orgzly')
  call minpac#add('vim-pandoc/vim-pandoc-syntax')
  call minpac#add('roxma/nvim-yarp')
  call minpac#add('ncm2/ncm2')
  call minpac#add('ncm2/ncm2-bufword')
  call minpac#add('ncm2/ncm2-path')
  call minpac#add('ncm2/ncm2-jedi')
  call minpac#add('filipekiss/ncm2-look.vim')
  call minpac#add('dracula/vim', {'name' : 'vim-dracula'})
endif

" }}}
