local fun = require("functions")
require("plugins")
_G["dump"] = fun.dump
vim.o["hidden"] = true
vim.o["termguicolors"] = true
vim.o["ignorecase"] = true
vim.o["smartcase"] = true
vim.o["inccommand"] = "nosplit"
vim.o["gdefault"] = true
vim.bo["expandtab"] = true
vim.bo["shiftwidth"] = 4
vim.bo["tabstop"] = 4
vim.bo["smartindent"] = true
vim.wo["number"] = true
vim.wo["wrap"] = false
for __0_, m_0_ in ipairs({"n"}) do
  local function _0_()
    return fun["make-fennel"]()
  end
  vim.api.nvim_set_keymap(m_0_, "gm", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_0_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  local function _0_()
    return fun["telescope-find-here"]()
  end
  vim.api.nvim_set_keymap(m_0_, "ge", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_0_) .. "()") .. "<cr>"), {noremap = true})
end
local function _0_()
  return vim.highlight.on_yank({on_visual = false})
end
vim.api.nvim_exec(table.concat({"autocmd", "TextYankPost", "*", ("lua " .. (require("functions")).mkfn(_0_) .. "()")}, " "), false)
for __0_, m_0_ in ipairs({"n"}) do
  vim.api.nvim_set_keymap(m_0_, "s", "<C-w>", {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  vim.api.nvim_set_keymap(m_0_, "st", "<cmd>terminal<cr>i", {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  local function _1_()
    return fun.edit(fun.config("fnl/startup.fnl"))
  end
  vim.api.nvim_set_keymap(m_0_, "gcc", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_1_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  local function _1_()
    return fun.edit(fun.config("fnl/plugins.fnl"))
  end
  vim.api.nvim_set_keymap(m_0_, "gcp", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_1_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"n", "i"}) do
  local function _1_()
    return fun["save-buffer"]()
  end
  vim.api.nvim_set_keymap(m_0_, "<c-s>", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_1_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  local function _1_()
    return (require("telescope.builtin")).find_files()
  end
  vim.api.nvim_set_keymap(m_0_, "<c-p>", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_1_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  vim.api.nvim_set_keymap(m_0_, "gp", "\"+p", {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  local function _1_()
    return fun["end-of-line"]()
  end
  vim.api.nvim_set_keymap(m_0_, "L", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_1_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  local function _1_()
    return fun["beginning-of-line"]()
  end
  vim.api.nvim_set_keymap(m_0_, "H", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_1_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  local function _1_()
    return vim.cmd("Neogit")
  end
  vim.api.nvim_set_keymap(m_0_, "<space>g", ("<cmd>" .. ("lua " .. (require("functions")).mkfn(_1_) .. "()") .. "<cr>"), {noremap = true})
end
for __0_, m_0_ in ipairs({"t"}) do
  vim.api.nvim_set_keymap(m_0_, "<esc>", "<C-\\><C-n>", {noremap = true})
end
vim.api.nvim_command("augroup terminal")
vim.api.nvim_command("au!")
local function _1_()
  return vim.cmd("setlocal nonumber")
end
local function _2_()
  return vim.cmd("setlocal matchpairs=")
end
local function _3_()
  return vim.cmd("setlocal scrolloff=0")
end
do local _ = {vim.api.nvim_exec(table.concat({"autocmd", "TermOpen", "*", ("lua " .. (require("functions")).mkfn(_1_) .. "()")}, " "), false), vim.api.nvim_exec(table.concat({"autocmd", "TermOpen", "*", ("lua " .. (require("functions")).mkfn(_2_) .. "()")}, " "), false), vim.api.nvim_exec(table.concat({"autocmd", "TermOpen", "*", ("lua " .. (require("functions")).mkfn(_3_) .. "()")}, " "), false)} end
return vim.api.nvim_command("augroup END")
