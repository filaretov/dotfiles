_G.v = require('v')

require('plugins')

-- Options
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
v.map('n', 'st', '<cmd>terminal<cr>i')
v.map('n', 'gb', ':<C-u>b ')
v.map('n', 'gcc', v.cmd("v.edit('$MYVIMRC')"))
v.map('n', 'gcp', v.cmd("v.edit(v.config('lua/plugins.lua'))"))
v.map('ni', '<c-s>', v.cmd("v.save_buffer()"))
v.map('n', '<c-h>', "<cmd>Telescope help_tags<cr>")
v.map('nv', 'L', '$')
v.map('nv', 'H', '^')
v.map('n', 'gl', '<cmd>luafile %<cr>')
v.map('n', '<space>g', "<cmd>Neogit<cr>")


-- Some ugly hacks for which we need VimScript
-- Otherwise known as calling lua from vimscript from lua
v.au("TextYankPost * lua vim.highlight.on_yank {on_visual = false}")

-- Terminal
v.map('t', '<esc>', '<C-\\><C-n>')
v.augroup(
"terminal",
{ 
    "TermOpen * setlocal nonumber",
    "TermOpen * setlocal matchpairs=",
    "TermOpen * setlocal scrolloff=0",
})
