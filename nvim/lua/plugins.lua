require("packer").startup(function()
  use('wbthomason/packer.nvim')
  use({ 'shaunsingh/nord.nvim', config = [[vim.cmd("colorscheme nord")]] })

  use({
    "kylechui/nvim-surround",
    tag = "*", -- Use for stability; omit to use `main` branch for the latest features
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end
  })

  --[[
attribute.inner
attribute.outer
block.inner
block.outer
call.inner
call.outer
class.inner
class.outer
comment.outer
conditional.inner
conditional.outer
frame.inner
frame.outer
function.inner
function.outer
loop.inner
loop.outer
parameter.inner
parameter.outer
scopename.inner
statement.outer
--]]

  use {
    'nvim-treesitter/nvim-treesitter',
    run = function() require('nvim-treesitter.install').update({ with_sync = true }) end,
    config = function() require('nvim-treesitter.configs').setup({
      ensure_installed = { 'rust', 'lua', 'rust', 'c', 'python' },
      highlight = { enable = true },
      indent = { enable = true},
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<C-n>",
          node_incremental = "<C-n>",
          scope_incremental = "<C-s>",
          node_decremental = "<C-r>",
        },
      },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            ["ic"] = "@class.inner",
            ["as"] = "@call.outer",
            ["is"] = "@call.inner",
            ["ab"] = "@block.outer",
            ["ib"] = "@block.inner",
            ["am"] = "@comment.outer",
            ["al"] = "@loop.outer",
            ["il"] = "@loop.inner",
            ["aa"] = "@attribute.outer",
            ["ia"] = "@attribute.inner",
            ["aP"] = "@parameter.outer",
            ["iP"] = "@parameter.inner",
          }
        }
      }
    })
    end
  }
  use 'nvim-treesitter/nvim-treesitter-textobjects'

  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
    requires = { {'nvim-lua/plenary.nvim'} },
    config = function()
      local actions = require('telescope.actions')
      require("telescope").setup{
        defaults = {
          mappings = {
            i = {
              ["<esc>"] = actions.close
            },
          },
        }
      }
    end
  }
end)
