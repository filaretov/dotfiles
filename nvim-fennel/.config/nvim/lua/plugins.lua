local packer = require("packer")
local function _0_()
  use({"wbthomason/packer.nvim"})
  local function _1_()
    return (require("nord")).set()
  end
  use({"shaunsingh/nord.nvim", config = _1_})
  use({"tpope/vim-surround"})
  use({"tpope/vim-repeat"})
  use({"tpope/vim-rsi"})
  use({"tpope/vim-fugitive"})
  use({"tommcdo/vim-exchange"})
  local function _2_()
    return (require("neogit")).setup()
  end
  use({"TimUntersberger/neogit", config = _2_, requires = {"nvim-lua/plenary.nvim"}})
  use({"bakpakin/fennel.vim"})
  local function _3_()
    return (require("gitsigns")).setup()
  end
  use({"lewis6991/gitsigns.nvim", config = _3_, requires = {"nvim-lua/plenary.nvim"}})
  use({"nvim-telescope/telescope.nvim", requires = {"nvim-lua/popup.nvim", "nvim-lua/plenary.nvim"}})
  local function _4_()
    return require("config.snippets")
  end
  return use({"norcalli/snippets.nvim", config = _4_, rocks = {"lyaml"}})
end
return packer.startup(_0_)
