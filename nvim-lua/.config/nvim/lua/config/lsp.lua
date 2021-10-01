local lsp = require('lspconfig')

local lsp_on_attach = function(client, bufnr)
    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    local function buf_set_option(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    -- Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = {
        noremap = true,
        silent = true
    }

    local function buf(fn)
        return string.format("<cmd>lua vim.lsp.buf.%s()<CR>", fn)
    end

    local function diag(fn)
        return string.format("<cmd>lua vim.lsp.diagnostic.%s()<CR>", fn)
    end

    buf_set_keymap('i', '<C-Space>', buf('completion'), opts)

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    buf_set_keymap('n', 'gD', buf('declaration'), opts)
    buf_set_keymap('n', 'gd', buf('definition'), opts)
    buf_set_keymap('n', 'K', buf('hover'), opts)
    buf_set_keymap('n', 'gi', buf('implementation'), opts)
    buf_set_keymap('n', '<C-k>', buf('signature_help'), opts)
    buf_set_keymap('n', '<space>wa', buf('add_workspace_folder'), opts)
    buf_set_keymap('n', '<space>wr', buf('remove_workspace_folder'), opts)
    buf_set_keymap('n', '<space>wl', buf('list_workspace_folders'), opts)
    buf_set_keymap('n', '<space>D', buf('type_definition'), opts)
    buf_set_keymap('n', '<space>rn', buf('rename'), opts)
    buf_set_keymap('n', '<space>ca', buf('code_action'), opts)
    buf_set_keymap('n', 'gr', buf('references'), opts)
    buf_set_keymap('n', '<space>e', diag('show_line_diagnostics'), opts)
    buf_set_keymap('n', '[d', diag('goto_prev'), opts)
    buf_set_keymap('n', ']d', diag('goto_next'), opts)
    buf_set_keymap('n', '<space>q', diag('set_loclist'), opts)
    buf_set_keymap('n', '<space>f', buf('formatting'), opts)

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
end

local servers = {"pyright", "rust_analyzer", "texlab"}
for _, server in ipairs(servers) do
    lsp[server].setup {
        autostart = false,
        on_attach = lsp_on_attach,
        flags = {
            debounce_text_changes = 150
        }
    }
end

