local utils = {}

local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

function utils.opt(scope, key, value)
    scopes[scope][key] = value
    if scope ~= 'o' then scopes['o'][key] = value end
end

function utils.map(mode, lhs, rhs, opts)
    local options = {noremap = true}
    if opts then options = vim.tbl_extend('force', options, opts) end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

function utils.copy(t)
    local new = {}
    for k, v in pairs(t) do
        new[k] = v
    end
    return new
end

function utils.cmd(fun)
    return "<cmd>lua " .. fun .. "<cr>"
end

function utils.config(filename)
    return vim.fn.stdpath("config") .. "/" .. filename
end

function utils.edit(filename)
    vim.cmd("edit " .. filename)
end

function utils.save_buffer()
    vim.cmd("w")
end

function utils.eol()
    vim.cmd("normal $")
end

function utils.bol()
    vim.cmd("normal ^")
end

return utils
