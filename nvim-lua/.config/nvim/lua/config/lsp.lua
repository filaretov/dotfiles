local lsp = require('lspconfig')

local lsp_on_attach = function(client, bufnr)
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
end

local servers = {"pyright", "rust_analyzer", "texlab"}
for _, server in ipairs(servers) do
    lsp[server].setup { on_attach = lsp_on_attach }
end

