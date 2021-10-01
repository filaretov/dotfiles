_G.v = require('v')

require("maps")
require("plugins")
require("nord").set()

local o = vim.o
local bo = vim.bo
local wo = vim.wo

-- Options
o.hidden =  true
o.termguicolors =  true
o.ignorecase =  true
o.smartcase =  true
o.wildmode =  'list:longest'
o.completeopt =  'menuone,noselect'
o.grepprg = "rg --vimgrep --no-heading"

-- Buffer local
local indent = 4
bo.expandtab =  true
bo.shiftwidth =  indent
bo.tabstop =  indent
bo.smartindent =  true

-- Window local
wo.wrap = false

wo.foldmethod = "expr"
wo.foldexpr = "nvim_treesitter#foldexpr()"
wo.foldtext= [[getline(v:foldstart).'...'.getline(v:foldend)]]
wo.fillchars = "fold: "
wo.foldnestmax = 2
o.foldlevelstart = 99

wo.signcolumn = "yes"
wo.number = true

-- Augroups
-- Otherwise known as calling lua from vimscript from lua
v.au("TextYankPost * lua vim.highlight.on_yank {on_visual = false}")

v.augroup(
"terminal",
{ 
    "TermOpen * setlocal nonumber",
    "TermOpen * setlocal matchpairs=",
    "TermOpen * setlocal scrolloff=0",
})
