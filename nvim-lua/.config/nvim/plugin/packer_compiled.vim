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
  gruvbox = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/gruvbox"
  },
  ["julia-vim"] = {
    loaded = true,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/start/julia-vim"
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
  ["packer.nvim"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/hgf/.local/share/nvim/site/pack/packer/opt/packer.nvim"
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
  }
}

-- Config for: telescope.nvim
require('config.telescope')
-- Config for: nvim-lspconfig
require('config.lsp')
-- Config for: nvim-treesitter
require('config.treesitter')
-- Config for: snippets.nvim
require('config.snippets')
END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
