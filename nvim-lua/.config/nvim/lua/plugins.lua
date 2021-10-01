require("packer").startup(function()
    use {"wbthomason/packer.nvim"}
    use {
        "shaunsingh/nord.nvim",
        config = function() require("nord").set() end
    }

    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function()
            require("nvim-treesitter.configs").setup({
                ensure_installed = "maintained",
                highlight = {
                    enable = true
                }
            })
        end
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

    -- A file tree
    use {
        'kyazdani42/nvim-tree.lua',
        requires = 'kyazdani42/nvim-web-devicons',
        config = function() require'nvim-tree'.setup {} end
    }


    use {
        "timuntersberger/neogit",
        requires = {'nvim-lua/plenary.nvim'}
        -- This makes neovim crash on startup with a filename
        -- argument, which is not great
        -- config = function() require("neogit").setup() end
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

    -- Rust
    use {
        "simrat39/rust-tools.nvim",
        requires = {"nvim-lua/popup.nvim", "nvim-lua/plenary.nvim"},
        config = function() require('rust-tools').setup({}) end
    }
end)

