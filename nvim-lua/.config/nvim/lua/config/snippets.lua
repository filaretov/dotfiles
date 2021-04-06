local map = require('v').map

map('i', '<c-j>', [[<cmd>lua return require'snippets'.expand_or_advance(1)<cr>]])
map('i', '<c-k>', [[<cmd>lua return require'snippets'.advance_snippet(-1)<CR>]])
map('n', 'gs', [[<cmd>lua vim.cmd('edit '..vim.fn.stdpath('config')..'/lua/config/snippets.lua')<cr>]])

require('snippets').snippets = {
    _global = {
        todo = "TODO(hgf): ";
    },
    rust = {
        ddbg = "#[derive(Debug)]";
        pn = [[println!("$0")]];
    },
    python = {
        main = [[if __name__ == "__main__":
    $0]];
        ipy = "from IPython import embed; embed()";
    }
}
