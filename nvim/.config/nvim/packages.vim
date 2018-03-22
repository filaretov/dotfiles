command! PackUpdate packadd minpac | source $MYVIMRC | redraw | call minpac#update()
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()

if !exists('*minpac#init')
      finish
endif

call minpac#init()
call minpac#add('k-takata/minpac', {'type': 'opt'})

call minpac#add('equalsraf/neovim-gui-shim')
call minpac#add('kana/vim-textobj-entire')
call minpac#add('kana/vim-textobj-user')
call minpac#add('kballard/vim-fish')
call minpac#add('lilydjwg/colorizer')
call minpac#add('machakann/vim-sandwich')
call minpac#add('rust-lang/rust.vim')
call minpac#add('Shougo/neosnippet.vim')
call minpac#add('tommcdo/vim-exchange')
call minpac#add('tpope/vim-repeat')
call minpac#add('vim-pandoc/vim-pandoc')
call minpac#add('vim-pandoc/vim-pandoc-syntax')
call minpac#add('w0rp/ale')
call minpac#add('arcticicestudio/nord-vim')
