local v = {}

local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

function v.opt(scope, key, value)
    scopes[scope][key] = value
    if scope ~= 'o' then scopes['o'][key] = value end
end

function v.map(modes, lhs, rhs, opts)
    local options = {noremap = true}
    if opts then options = vim.tbl_extend('force', options, opts) end
    for c in modes:gmatch(".") do
        vim.api.nvim_set_keymap(c, lhs, rhs, options)
    end
end

function v.copy(t)
    local new = {}
    for k, v in pairs(t) do
        new[k] = v
    end
    return new
end

function v.tmap(tbl, f)
    local t = {}
    for k,v in pairs(tbl) do
        t[k] = f(v)
    end
    return t
end

function v.cmd(fun)
    return "<cmd>lua " .. fun .. "<cr>"
end

function v.config(filename)
    return vim.fn.stdpath("config") .. "/" .. filename
end

function v.edit(filename)
    vim.cmd("edit " .. filename)
end

function v.save_buffer()
    vim.cmd("w")
end

function v.eol()
    vim.cmd("normal $")
end

function v.bol()
    vim.cmd("normal ^")
end

function v.au(directive)
    local cmd = "autocmd " .. directive
    vim.api.nvim_exec(cmd, false)
end

function v.augroup(name, cmds)
    local open = "augroup " .. name
    local clear = "autocmd!"
    local aus = table.concat(v.tmap(cmds, function(s) return ("autocmd " .. s) end), "\n")
    local close = "augroup end"
    local cmd = table.concat({open, clear, aus, close}, "\n")
    vim.api.nvim_exec(cmd, false)
end

return v
