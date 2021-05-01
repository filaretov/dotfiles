local map = require('v').map

require('compe').setup {
    enabled = true;
    autocomplete = true;
    debug = false;
    documentation = true;

    source = {
        path = true;
        buffer = true;
        nvim_lsp = true;
        nvim_lua = true;
    }
}

map('i', '<C-Space>', 'compe#complete()', {expr = true, silent = true})
