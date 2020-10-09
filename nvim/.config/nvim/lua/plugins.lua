vim.cmd [[packadd packer.nvim]]
vim._update_package_paths()

return require('packer').startup(function()
    use {'wbthomason/packer.nvim', opt = true}

    --- Filetypes
    use {'rust-lang/rust.vim'}
    use {'kballard/vim-fish'}
    use {'elixir-editors/vim-elixir'}
    use {'vim-pandoc/vim-pandoc-syntax'}
    use {'cespare/vim-toml'}
    use {'ledger/vim-ledger'}
    use 'fatih/vim-go'

    --- UI
    use {'arcticicestudio/nord-vim'}
    use {'romainl/flattened'}

    --- Goodies
    use {'sgur/vim-editorconfig'}
    use {'shougo/neosnippet.vim'}
    use {'tommcdo/vim-exchange'}
    use {'junegunn/fzf', run = ':call fzf#install()'}
    use {'junegunn/fzf.vim'}
    use {'tpope/vim-abolish'}
    use {'tpope/vim-commentary'}
    use {'tpope/vim-eunuch'}
    use {'tpope/vim-fugitive'}
    use {'tpope/vim-repeat'}
    use {'tpope/vim-rsi'}
    use {'tpope/vim-surround'}
    use {'tpope/vim-unimpaired'}
    use {'neomake/neomake'}
end)

