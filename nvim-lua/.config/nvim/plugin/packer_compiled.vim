" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
local package_path_str = "/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/hgf/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    print('Error running ' .. component .. ' for ' .. name)
    error(result)
  end
  return result
end

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
  ["hecate-theme"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/hecate-theme"
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
  ["lush.nvim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/lush.nvim"
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

-- Config for: telescope.nvim
require('config.telescope')
-- Config for: snippets.nvim
require('config.snippets')
-- Config for: lsp-trouble.nvim
try_loadstring("\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\ftrouble\frequire\0", "config", "lsp-trouble.nvim")
-- Config for: nvim-treesitter
require('config.treesitter')
-- Config for: gitsigns.nvim
require('config.gitsigns')
-- Config for: nvim-compe
require('config.compe')
-- Config for: nvim-lspconfig
require('config.lsp')
END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
