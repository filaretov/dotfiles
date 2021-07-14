local fun = require("functions")
local yaml = require("lyaml")
local snippets_path = fun.config("snips.yml")
for __0_, m_0_ in ipairs({"i"}) do
  vim.api.nvim_set_keymap(m_0_, "<c-j>", "<cmd>lua return require'snippets'.expand_or_advance(1)<cr>", {noremap = true})
end
for __0_, m_0_ in ipairs({"i"}) do
  vim.api.nvim_set_keymap(m_0_, "<c-k>", "<cmd>lua return require'snippets'.advance_snippet(-1)<CR>", {noremap = true})
end
for __0_, m_0_ in ipairs({"n"}) do
  vim.api.nvim_set_keymap(m_0_, "gs", "<cmd>lua vim.cmd('edit '..vim.fn.stdpath('config')..'/snips.yml')<cr>", {noremap = true})
end
local function set_snippets()
  local fin = io.open(snippets_path)
  local function close_handlers_0_(ok_0_, ...)
    fin:close()
    if ok_0_ then
      return ...
    else
      return error(..., 0)
    end
  end
  local function _0_()
    local snippets = yaml.load(fin:read("*all"))
    require("snippets")["snippets"] = snippets
    return nil
  end
  return close_handlers_0_(xpcall(_0_, (package.loaded.fennel or debug).traceback))
end
do
  vim.api.nvim_command("augroup snippets")
  vim.api.nvim_command("au!")
  local function _0_()
    return set_snippets()
  end
  do local _ = {vim.api.nvim_exec(table.concat({"autocmd", "BufWritePost", "snips.yml", ("lua " .. (require("functions")).mkfn(_0_) .. "()")}, " "), false)} end
  vim.api.nvim_command("augroup END")
end
return set_snippets()
