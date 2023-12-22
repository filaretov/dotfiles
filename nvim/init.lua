-- Plugins
-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
  "ellisonleao/gruvbox.nvim",
  "tpope/vim-abolish",
  "tpope/vim-eunuch",
  "tpope/vim-rsi",
  "tpope/vim-repeat",
  "tpope/vim-unimpaired",
  "tommcdo/vim-exchange",
  {'aymericbeaumet/vim-symlink', dependencies = { 'moll/vim-bbye' }},
  {"kylechui/nvim-surround", config = true },
  -- git stuff
  "tpope/vim-fugitive",
  "tpope/vim-rhubarb",
  {"lewis6991/gitsigns.nvim", dependencies = { "nvim-lua/plenary.nvim" }, config = true},
  -- visual ui stuff
  "lukas-reineke/indent-blankline.nvim",
  "stevearc/dressing.nvim",
  -- colorscheme
  "folke/tokyonight.nvim",
  -- completion
  "hrsh7th/cmp-nvim-lsp",
  "hrsh7th/nvim-cmp",
  -- treesitter
  { "nvim-telescope/telescope.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  config = function()
    local actions = require('telescope.actions')
    require('telescope').setup({
      defaults = {
        mappings = {
          i = {
            ["<esc>"] = actions.close
          }
        }
      }
    })
  end
}
})

-- LSP
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<space>f', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
})

-- Completion
local cmp = require("cmp")

cmp.setup({
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
  }, {
    { name = 'buffer' },
  }),
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }),
})

-- Keymaps
vim.keymap.set('n', 'spl', '<cmd>:luafile %<cr>')
vim.keymap.set('n', 'sf', '<cmd>:write<cr>')
vim.keymap.set('n', 'st', '<cmd>:te nu<cr>')
vim.keymap.set('n', 'sh', '<C-w>h')
vim.keymap.set('n', 'sj', '<C-w>j')
vim.keymap.set('n', 'sk', '<C-w>k')
vim.keymap.set('n', 'sl', '<C-w>l')
vim.keymap.set('n', 'sv', '<C-w>v')
vim.keymap.set('n', 'ss', '<C-w>s')
vim.keymap.set('n', 'sq', '<C-w>q')
vim.keymap.set('t', '<esc>', '<C-\\><C-n>')
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)

vim.cmd('colorscheme gruvbox')
