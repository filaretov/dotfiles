require("plugins")

local function edit(filename)
  return function() vim.cmd("edit " .. filename) end
end

local function config(filename)
  return vim.fs.normalize(vim.fn.stdpath("config") .. "/" .. filename)
end

vim.cmd("colorscheme nord")
vim.o.number = true
vim.o.foldlevelstart = 99
vim.o.signcolumn = "yes"
vim.keymap.set('t', '<esc>', '<C-\\><C-n>')
vim.keymap.set('n', 'st', '<cmd>:te nu<cr>')
vim.keymap.set('n', 'sf', '<cmd>:w<cr>')
vim.keymap.set('n', 'gl', '<cmd>:luafile %<cr>')
vim.keymap.set('n', 's', '<C-w>', { remap = false })
vim.keymap.set('n', 'gcd', '<cmd>cd %:h<cr>')
vim.keymap.set('n', 'gcc', edit(config("init.lua")))
vim.keymap.set('n', 'gcp', edit(config("lua/plugins.lua")))

-- Telescope maps
local function project_files()
  local opts = {}
  vim.fn.system('git rev-parse --is-inside-work-tree')
  if vim.v.shell_error == 0 then
    require"telescope.builtin".git_files(opts)
  else
    require"telescope.builtin".find_files(opts)
  end
end
local telescope = require('telescope.builtin')
vim.keymap.set('n', 'go', project_files)
vim.keymap.set('n', 'gpr', telescope.live_grep)
vim.keymap.set('n', '<C-b>', telescope.buffers)
vim.keymap.set('n', 'gpb', telescope.buffers)
vim.keymap.set('n', '<C-h>', telescope.help_tags)
vim.keymap.set('n', 'gph', telescope.help_tags)
vim.keymap.set('n', 'gpt', telescope.treesitter)

vim.keymap.set('v', 'v', '<C-v>')


vim.cmd([[
augroup neovim_terminal
autocmd!
autocmd TermOpen * startinsert
autocmd TermOpen * :set nonumber norelativenumber signcolumn=no
autocmd TermOpen * nnoremap <buffer> <C-c> i<C-c><C-\><C-n>
augroup END
]])


vim.cmd([[
function! FoldConfig()
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
endfunction

autocmd BufAdd,BufEnter,BufNew,BufNewFile,BufWinEnter * :call FoldConfig()
]])

vim.cmd([[
  augroup packer_user_config
  autocmd!
  autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])
