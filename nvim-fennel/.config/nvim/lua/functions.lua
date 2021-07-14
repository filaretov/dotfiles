__nvim_functions__ = {}
local vim = _G.vim
local function mkfn(func)
  local idx = (#__nvim_functions__ + 1)
  __nvim_functions__[idx] = func
  return ("__nvim_functions__[" .. idx .. "]")
end
local function map(f, tbl)
  local new_tbl = {}
  for _, v in ipairs(tbl) do
    table.insert(new_tbl, f(v))
  end
  return new_tbl
end
local function nil_3f(v)
  return (nil == v)
end
local function dump(...)
  for _, v in ipairs({...}) do
    print(vim.inspect(v))
  end
  return nil
end
local function make_fennel()
  local config = vim.fn.stdpath("config")
  vim.cmd(("!make --makefile " .. config .. "/makefile " .. "--directory " .. config))
  return vim.cmd(table.concat({"luafile", (config .. "/lua/startup.lua")}, " "))
end
local function telescope_find_here()
  local fun = (require("telescope.builtin")).find_files
  return fun({search_dirs = {_G.vim.fn.expand("%:p:h")}})
end
local function edit(filename)
  return vim.cmd(("edit " .. filename))
end
local function normal(cmd)
  return vim.cmd(("normal " .. cmd))
end
local function config(filename)
  return (vim.fn.stdpath("config") .. "/" .. filename)
end
local function end_of_line()
  return normal("$")
end
local function beginning_of_line()
  return normal("^")
end
local function save_buffer()
  return vim.cmd("w")
end
return {["beginning-of-line"] = beginning_of_line, ["end-of-line"] = end_of_line, ["make-fennel"] = make_fennel, ["nil?"] = nil_3f, ["save-buffer"] = save_buffer, ["telescope-find-here"] = telescope_find_here, config = config, dump = dump, edit = edit, map = map, mkfn = mkfn}
