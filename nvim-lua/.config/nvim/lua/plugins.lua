require("packer").startup(function()
    use {"wbthomason/packer.nvim"}
    use {"folke/lsp-colors.nvim"}
    use {
        "shaunsingh/nord.nvim",
        config = function() require("nord").set() end
    }

    -- The basics
    use {"tpope/vim-surround"}
    use {"tpope/vim-eunuch"}
    use {"tpope/vim-rsi"}
    use {"tpope/vim-repeat"}
    use {"tommcdo/vim-exchange"}

    -- common dependencies
    use {"nvim-lua/plenary.nvim"}
    use {"nvim-lua/popup.nvim"}
    use {"kyazdani42/nvim-web-devicons"}

    -- Fun UI stuff
    use {
        "timuntersberger/neogit",
        requires = {'nvim-lua/plenary.nvim'},
        config = function() require("neogit").setup() end
    }
    use {
        "lewis6991/gitsigns.nvim",
        requires = {'nvim-lua/plenary.nvim'},
        config = function() require("gitsigns").setup() end
    }
    use {
        "nvim-telescope/telescope.nvim",
        requires = {'nvim-lua/plenary.nvim'},
        config = function() require('config.telescope') end
    }

    -- LSP
    use {
        "neovim/nvim-lspconfig",
        config = function() require("config.lsp") end
    }
    use {
        "hrsh7th/nvim-compe",
        config = function() require("config.compe") end
    }

    -- Filetypes
    use {"kristijanhusak/orgmode.nvim"}

    -- Neovim
    use {"folke/lua-dev.nvim"}
    use {"rafcamlet/nvim-luapad"}
end)

