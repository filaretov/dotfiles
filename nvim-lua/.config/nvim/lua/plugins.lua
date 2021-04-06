local v = require('v')

local packer = require('packer')

packer.init()
packer.reset()

local use = packer.use

use {'wbthomason/packer.nvim', opt = true}
use {'morhetz/gruvbox'}
use {'tpope/vim-surround'}
use {'tpope/vim-eunuch'}
use {'tpope/vim-rsi'}
use {'tpope/vim-fugitive'}
use {'nvim-treesitter/nvim-treesitter', config = [[require('config.treesitter')]]}
use {'neovim/nvim-lspconfig', config = [[require('config.lsp')]]}

use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = [[require('config.telescope')]],
}

use {'nvim-lua/completion-nvim'}
use {'norcalli/snippets.nvim', config = [[require('config.snippets')]]}

use {'JuliaEditorSupport/julia-vim'}
