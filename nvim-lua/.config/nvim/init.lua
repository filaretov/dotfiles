local v = require('v')

require('ensure_packer')
require('plugins')

-- Options
vim.cmd 'colorscheme tokyonight'
v.opt('o', 'hidden', true)
v.opt('o', 'termguicolors', true)
v.opt('o', 'ignorecase', true)
v.opt('o', 'smartcase', true)
v.opt('o', 'wildmode', 'list:longest')
v.opt('o', 'completeopt', 'menuone,noselect')

-- Buffer local
local indent = 4
v.opt('b', 'expandtab', true)
v.opt('b', 'shiftwidth', indent)
v.opt('b', 'tabstop', indent)
v.opt('b', 'smartindent', true)

-- Window local
v.opt('w', 'wrap', false)

-- Mappings
-- Normal mode
v.map('n', 's', '<C-w>')
v.map('n', 'st', '<cmd>terminal<cr>')
v.map('n', 'sT', '<cmd>split | execute "normal <c-w>j" | terminal<cr>')
v.map('n', 'ge', ':<C-u>edit %:h')
v.map('n', 'gc', '<cmd>edit $MYVIMRC<cr>')
v.map('n', 'gb', ':<C-u>b ')
v.map('n', '<c-s>', '<cmd>w<cr>')
v.map('n', '<c-h>', ':help ')

-- Terminal
v.map('t', '<esc>', '<C-\\><C-n>')

-- Some ugly hacks we need VimScript for
-- Otherwise known as calling lua from vimscript from lua
vim.cmd 'au TextYankPost * lua vim.highlight.on_yank {on_visual = false}'
