-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["completion-nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/completion-nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "require('config.gitsigns')" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  gruvbox = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/gruvbox"
  },
  ["julia-vim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/julia-vim"
  },
  ["lsp-colors.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/lsp-colors.nvim"
  },
  ["lsp-trouble.nvim"] = {
    config = { "\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\ftrouble\frequire\0" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/lsp-trouble.nvim"
  },
  ["lua-dev.nvim"] = {
    config = { "require('config.luadev')" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/lua-dev.nvim"
  },
  ["lush.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/lush.nvim"
  },
  neovim = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/neovim"
  },
  ["nord.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/nord.nvim"
  },
  ["nvim-compe"] = {
    config = { "require('config.compe')" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-lspconfig"] = {
    config = { "require('config.lsp')" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-luapad"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/nvim-luapad"
  },
  ["nvim-treesitter"] = {
    config = { "require('config.treesitter')" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  playground = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/playground"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["snippets.nvim"] = {
    config = { "require('config.snippets')" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/snippets.nvim"
  },
  ["telescope.nvim"] = {
    config = { "require('config.telescope')" },
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["tokyonight.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/tokyonight.nvim"
  },
  ["vim-eunuch"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/vim-eunuch"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-rsi"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/vim-rsi"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/vim-surround"
  },
  ["weatherwax-theme"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/weatherwax-theme"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: lua-dev.nvim
time([[Config for lua-dev.nvim]], true)
require('config.luadev')
time([[Config for lua-dev.nvim]], false)
-- Config for: snippets.nvim
time([[Config for snippets.nvim]], true)
require('config.snippets')
time([[Config for snippets.nvim]], false)
-- Config for: nvim-lspconfig
time([[Config for nvim-lspconfig]], true)
require('config.lsp')
time([[Config for nvim-lspconfig]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
require('config.telescope')
time([[Config for telescope.nvim]], false)
-- Config for: lsp-trouble.nvim
time([[Config for lsp-trouble.nvim]], true)
try_loadstring("\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\ftrouble\frequire\0", "config", "lsp-trouble.nvim")
time([[Config for lsp-trouble.nvim]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
require('config.gitsigns')
time([[Config for gitsigns.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
require('config.treesitter')
time([[Config for nvim-treesitter]], false)
-- Config for: nvim-compe
time([[Config for nvim-compe]], true)
require('config.compe')
time([[Config for nvim-compe]], false)
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
