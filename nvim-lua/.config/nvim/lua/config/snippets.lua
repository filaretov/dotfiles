local map = require('v').map

vim.api.nvim_exec([[
augroup Snippets
  autocmd!
  autocmd BufWritePost snippets.lua luafile %
augroup end
]], false)

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
        ifdbg = "#[cfg(debug_assertions)]";
    },
    python = {
        main = [[if __name__ == "__main__":
    $0]];
        ipy = "from IPython import embed; embed()";
    },
    c = {
        hin = [[#include "$0"]];
        sin = [[#include <$0>]];
        hg  = [[
#ifndef ${-1=vim.fn.expand("%:t:r"):upper() .. "_H"}
#define ${-1}

#endif // ${-1}]];
        u8 = "uint8_t";
        u32 = "uint32_t";
        ch = "chunk";
        Ch = "Chunk";
        st = "size_t";
    },
    tex = {
        bb = "\\begin{${2:$1}}\n$0\n\\end{$2}";
    }
}
