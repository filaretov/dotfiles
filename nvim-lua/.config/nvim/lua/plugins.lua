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
    use {"tpope/vim-abolish"}
    use {"tpope/vim-fugitive"}
    use {"tpope/vim-unimpaired"}
    use {"tommcdo/vim-exchange"}

    -- common dependencies
    use {"nvim-lua/plenary.nvim"}
    use {"nvim-lua/popup.nvim"}

    -- fzf
    use {'vijaymarupudi/nvim-fzf'}


    use {
        "lewis6991/gitsigns.nvim",
        requires = {'nvim-lua/plenary.nvim'},
        config = function() require("gitsigns").setup() end
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

