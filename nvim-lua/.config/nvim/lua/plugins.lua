local install_path = vim.fn.stdpath('data') .. '/site/pack/packerstart/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.api.nvim_command('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.api.nvim_exec([[
augroup Packer
  autocmd!
  autocmd BufWritePost plugins.lua luafile %
  autocmd BufWritePost plugins.lua PackerCompile
augroup end
]], false)

return require('packer').startup(function()

use {'wbthomason/packer.nvim'}

-- colorscheme
use {'morhetz/gruvbox'}
use {'~/dev/yaks/weatherwax-theme'}
use {'~/dev/yaks/hecatheme/neovim'}
use {'folke/lsp-colors.nvim'}
use {'rktjmp/lush.nvim'}
use {'folke/tokyonight.nvim'}
use {'shaunsingh/nord.nvim'}

-- tpope stuff
use {'tpope/vim-surround'}
use {'tpope/vim-eunuch'}
use {'tpope/vim-rsi'}
use {'tpope/vim-fugitive'}

-- syntax highlighting and objects
use {'nvim-treesitter/nvim-treesitter', config = [[require('config.treesitter')]]}
use {'nvim-treesitter/playground'}

-- LSP
use {'neovim/nvim-lspconfig', config = [[require('config.lsp')]]}

use {
  "folke/lsp-trouble.nvim",
  requires = "kyazdani42/nvim-web-devicons",
  config = function()
    require("trouble").setup()
  end
}

use {'nvim-lua/completion-nvim'}

use {'hrsh7th/nvim-compe', config = [[require('config.compe')]]}

use {'norcalli/snippets.nvim', config = [[require('config.snippets')]]}

-- UI improvements
use {
    'lewis6991/gitsigns.nvim',
    requires = {'nvim-lua/plenary.nvim'},
    config = [[require('config.gitsigns')]],
}

-- Telescope
use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}},
    config = [[require('config.telescope')]],
}

-- Neovimming
use {"rafcamlet/nvim-luapad"}
use { "folke/lua-dev.nvim", config = [[require('config.luadev')]], }

-- Rust

-- Python

-- Julia
use {'JuliaEditorSupport/julia-vim'}

end)

