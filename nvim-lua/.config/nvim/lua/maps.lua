local v = require("v")

-- Mappings
-- Normal mode
v.map('n', 's', '<C-w>')
v.map('n', 'st', '<cmd>terminal nu<cr>i')
v.map('n', 'gb', ':<C-u>b ')
v.map('n', 'gcc', v.cmd("v.edit('$MYVIMRC')"))
v.map('n', 'gcp', v.cmd("v.edit(v.config('lua/plugins.lua'))"))
v.map('n', 'gcm', v.cmd("v.edit(v.config('lua/maps.lua'))"))
v.map('n', 'gco', v.cmd("v.edit(v.config('lua/options.lua'))"))
v.map('n', 'gcs', "<cmd>luafile $MYVIMRC<cr>")
v.map('ni', '<c-s>', v.cmd("v.save_buffer()"))
v.map('nv', 'L', '$')
v.map('nv', 'H', '^')
v.map('n', 'gl', '<cmd>luafile %<cr>')
v.map('n', '<space>g', "<cmd>Neogit<cr>")
v.map('n', '<Tab>', 'za')
v.map('i', '<C-v>', '<C-r>"')


-- Terminal
v.map('t', '<esc>', '<C-\\><C-n>')
