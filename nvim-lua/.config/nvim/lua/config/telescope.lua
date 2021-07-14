local v = require'v'

function _G.telescope_find_here()
    local here = vim.fn.expand("%:p:h")
    require('telescope.builtin').find_files({search_dirs = {here}})
end

v.map('n', '<C-p>', '<cmd>Telescope find_files<cr>')
v.map('n', 'ge', '<cmd>lua telescope_find_here()<cr>')

