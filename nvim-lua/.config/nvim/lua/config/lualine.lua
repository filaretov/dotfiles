local c_nord = require'lualine.themes.nord'
c_nord.inactive.c.bg = c_nord.normal.c.bg -- rgb colors are supported
c_nord.inactive.c.fg = c_nord.normal.c.fg -- rgb colors are supported
require("lualine").setup {

    options = {
        icons_enabled = false,
        theme = c_nord,
        section_separators = "",
        component_separators = "",
        lower = true,
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {}
    },
    sections = {
        lualine_x = {'encoding', 'filetype'},
        lualine_y = {},
        lualine_z = {},
    },
}

