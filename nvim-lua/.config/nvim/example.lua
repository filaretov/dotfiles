local v = require("v")
local fzf = require('fzf')

fzf.default_options = { relative = "win" }

_G.fzf_open_file = function()
	local result = fzf.fzf("fd")
	if result then
		vim.cmd(string.format("edit %s", result[1]))
	end
end

local function co_fzf (fn)
    return string.format(":lua coroutine.wrap(%s)()<cr>", fn)
end

v.map("n", "<C-p>", co_fzf("fzf_open_file"))
