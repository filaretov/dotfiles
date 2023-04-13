require("plugins")

function pp(arg)
  print(vim.inspect(arg))
end

local function trim(s)
  return s:gsub("^%s*(.-)%s*$", "%1")
end

function ssh_to_https(url)
    -- Check if the URL is an SSH Git URL
    if url:find("git@") == 1 then
        -- Replace the ":" separator with a "/"
        url = url:gsub(":", "/")
        -- Replace the "git@" prefix with "https://"
        url = url:gsub("git@", "https://")
        -- Remove the ".git" suffix, if present
        url = url:gsub("%.git$", "")
    end
    -- Return the modified URL
    return url
end


local function system(s)
  return trim(vim.fn.system(s))
end


function open_github_line()
  -- Get the URL of the current Git repository
  local repo_url = vim.fn.systemlist('git config --get remote.origin.url')[1]

  -- Check if the current file is part of a Git repository
  if repo_url == '' then
    print('Error: Current file is not part of a Git repository.')
    return
  end

  -- Get the relative path of the current file
  local file_path = vim.fn.resolve(vim.fn.expand('%:p')):sub(#vim.fn.systemlist('git rev-parse --show-toplevel')[1] + 2)

  -- Get the current line number
  local line_number = vim.fn.line('.')

  -- Open the URL for the current line in GitHub
  local partial = ssh_to_https(repo_url)
  local github_url = partial .. '/blob/' .. system('git rev-parse HEAD') .. '/' .. file_path .. '#L' .. line_number
  vim.fn['jobstart']({'open', github_url})
end



local function edit(filename)
  return function() vim.cmd("edit " .. filename) end
end

local function config(filename)
  return vim.fs.normalize(vim.fn.stdpath("config") .. "/" .. filename)
end

vim.o.number = true
vim.o.foldlevelstart = 99
vim.o.signcolumn = "yes"
vim.keymap.set('t', '<esc>', '<C-\\><C-n>')
vim.keymap.set('n', 'st', '<cmd>:te nu<cr>')
vim.keymap.set('n', 'sf', '<cmd>:w<cr>')
vim.keymap.set('n', 'gl', '<cmd>:luafile %<cr>')
vim.keymap.set('n', 's', '<C-w>')
vim.keymap.set('n', 'gcd', '<cmd>cd %:h<cr>')
vim.keymap.set('n', 'gcc', edit(config("init.lua")))
vim.keymap.set('n', 'gcp', edit(config("lua/plugins.lua")))
vim.keymap.set('n', '<space>y', '"+y')
vim.keymap.set('v', '<space>y', '"+y')

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
