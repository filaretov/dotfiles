local luadev = require("lua-dev").setup({
    lspconfig = {
        cmd = {"lua-language-server"}
    }
})
local lspconfig = require("lspconfig")
lspconfig.sumneko_lua.setup(luadev)
