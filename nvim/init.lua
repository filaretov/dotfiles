-- Put this at the top of 'init.lua'
local path_package = vim.fn.stdpath('data') .. '/site'
local mini_path = path_package .. '/pack/deps/start/mini.nvim'
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    'git', 'clone', '--filter=blob:none',
    -- Uncomment next line to use 'stable' branch
    -- '--branch', 'stable',
    'https://github.com/echasnovski/mini.nvim', mini_path
  }
  vim.fn.system(clone_cmd)
  vim.cmd('packadd mini.nvim | helptags ALL')
end

vim.o.shell = vim.system({ "which", "nu" }, { text = true }):wait().stdout:gsub("\n", "")
vim.o.shellcmdflag = "--stdin --no-newline -c"
vim.o.shellpipe = "| tee { save %s }"
vim.o.shelltemp = false
-- Shouldn't be used outside of diffing with shelltemp=false
vim.o.shellredir = "out+err> %s"
-- No escaping, no quoting
vim.o.shellxescape = ""
vim.o.shellxquote = ""
vim.o.shellquote = ""

vim.o.cmdheight = 0
vim.o.numberwidth = 3
vim.o.signcolumn = 'yes:1'
vim.o.statuscolumn = '%l%s'

vim.o.completeopt = "menu,menuone,noinsert,popup,fuzzy"
vim.o.cursorline = true
vim.o.cursorlineopt = "number"
vim.o.number = true
vim.o.winborder = 'rounded'

vim.o.textwidth = 88
vim.o.shiftwidth = 4
vim.o.expandtab = true
vim.o.path = vim.o.path .. ",**"

vim.o.grepprg = "rg --vimgrep --no-heading"

vim.keymap.set('t', '<esc>', '<C-\\><C-n>')
vim.keymap.set('t', '<C-[>', '<C-\\><C-n>')
vim.keymap.set('n', 'st', ':te<cr>')
vim.keymap.set('n', '<D-s>', ':w<cr>')
vim.keymap.set('n', '<D-x>', ':x<cr>')
vim.keymap.set('i', '<D-s>', '<C-o>:w<cr>')

vim.keymap.set('n', 'grf', vim.lsp.buf.format, { desc = 'lsp: format' })

vim.keymap.set('v', '.', ':normal .<cr>')

vim.keymap.set('c', '<C-b>', '<left>')
vim.keymap.set('c', '<C-f>', '<right>')
vim.keymap.set('c', '<C-a>', '<home>')
-- Already default, but nice for consistency
vim.keymap.set('c', '<C-e>', '<end>')
-- The arrow keys do prefix-matching
vim.keymap.set('c', '<C-p>', '<up>')
vim.keymap.set('c', '<C-n>', '<down>')
-- I don't use virtual replace, but I do use the 'gr'-prefixed LSP bindings
vim.keymap.set('n', 'gr', '<nop>')

local sev = vim.diagnostic.severity

vim.diagnostic.config({
  virtual_text = true,
  virtual_lines = false,
  -- Do not use characters in the statuscolumn to display diagnostics, just
  -- hightlight the number differently. This way I can use the characters in
  -- the statuscolumn for other stuff, like gitsigns
  signs = {
    text = {
      [sev.ERROR] = '',
      [sev.WARN] = '',
      [sev.INFO] = '',
      [sev.HINT] = '',
    },
    numhl = {
      [sev.ERROR] = 'DiagnosticError',
      [sev.WARN] = 'DiagnosticWarn',
      [sev.INFO] = 'DiagnosticInfo',
      [sev.HINT] = 'DiagnosticHint',
    },
  },
})

local function string_split(inputstr, sep)
  if sep == nil then
    sep = "%s"
  end
  local t = {}
  for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
    table.insert(t, str)
  end
  return t
end


local function edit_diagnostic_config()
  -- 1. Create weird name for buffer
  -- 2. Create autocommand with callback that will set config to content of buffer
  -- 3. tabe the buffer
  local name = vim.fn.tempname() .. '.lua'
  -- local buf = vim.api.nvim_create_buf(false, true)
  -- vim.api.nvim_buf_set_name(buf, name)

  vim.print(name)
  vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
    pattern = { name },
    callback = function(ev)
      local content = table.concat(vim.api.nvim_buf_get_lines(ev.buf, 0, -1, false), '\n')
      local config = assert(loadstring(content))()
      vim.diagnostic.config(config)
      vim.api.nvim_buf_delete(ev.buf, {})
    end
  })
  vim.cmd(':tabe ' .. name)
  local buf = vim.api.nvim_get_current_buf()
  local config = vim.inspect(vim.diagnostic.config())
  config = '---@type vim.diagnostic.Opts\nreturn ' .. config
  local split = string_split(config, '\n')
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, split)
end

vim.keymap.set('n', 'grde', edit_diagnostic_config)

vim.keymap.set('n', '<D-.>', '<cmd>lua vim.lsp.buf.code_action()<CR>')
vim.keymap.set('n', '<D-,>', '<cmd>edit $MYVIMRC<cr>')
vim.keymap.set('n', '<D-S-,>', '<cmd>luafile $MYVIMRC<cr>')

vim.api.nvim_create_augroup("kitty", { clear = true })
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  group = "kitty",
  pattern = { "kitty.conf" },
  command = "silent !^kill -SIGUSR1 (pgrep -a kitty)",
})

vim.lsp.enable({ "luals", "nushell", "ruff", })

local add = require('mini.deps').add

add("lewis6991/gitsigns.nvim")
require("gitsigns").setup({})
