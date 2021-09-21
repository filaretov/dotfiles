local function r(name)
	return function() require(name) end
end

local function s(name)
	return function() require(name).setup() end
end

require("packer").startup(function()
    use {"wbthomason/packer.nvim"}
    use {"folke/lsp-colors.nvim"}
    use {"shaunsingh/nord.nvim", run=function() require("nord").set() end}

    -- The basics
    use {"tpope/vim-surround"}
    use {"tpope/vim-eunuch"}
    use {"tpope/vim-rsi"}
    use {"tpope/vim-fugitive"}
    use {"tpope/vim-repeat"}
    use {"tommcdo/vim-exchange"}

    -- common dependencies
    use {"nvim-lua/plenary.nvim"}
    use {"nvim-lua/popup.nvim"}
    use {"kyazdani42/nvim-web-devicons"}

    -- Fun UI stuff
    use {"timuntersberger/neogit"}
    use {"hoob3rt/lualine.nvim"}
    use {"lewis6991/gitsigns.nvim", requires = {'nvim-lua/plenary.nvim'}, config = s"gitsigns"}
    use {"nvim-telescope/telescope.nvim", run=r"config.telescope"}

    -- LSP
    use {"neovim/nvim-lspconfig", run=r"config.lsp"}
    use {"hrsh7th/nvim-compe", run=r"config.compe"}

    -- Filetypes
    use {"kristijanhusak/orgmode.nvim"}

    -- Neovim
    use {"folke/lua-dev.nvim"}
    use {"rafcamlet/nvim-luapad"}
end)

