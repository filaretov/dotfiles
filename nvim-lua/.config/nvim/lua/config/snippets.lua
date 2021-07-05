local v = require('v')

vim.api.nvim_exec([[
augroup Snippets
autocmd!
autocmd BufWritePost snippets.lua luafile %
augroup end
]], false)

v.map('i', '<c-j>', [[<cmd>lua return require'snippets'.expand_or_advance(1)<cr>]])
v.map('i', '<c-k>', [[<cmd>lua return require'snippets'.advance_snippet(-1)<CR>]])
v.map('n', 'gs', [[<cmd>lua vim.cmd('edit '..vim.fn.stdpath('config')..'/lua/config/snippets.lua')<cr>]])

local c = {
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
    main = [[
    int main(void) {
        puts("Hello, world!");
        return EXIT_SUCCESS;
    }]]
}

local cpp = v.copy(c)

local python = {
    main = [[if __name__ == "__main__":
    $0]];
    ipy = "from IPython import embed; embed()";
}

local tex = {
    ac = "\\autocite{$0}";
    tc = "\\textcite{$0}";
    ci = "\\cite{$0}";
    re = "\\ref{$0}";
    ca = "\\caption{$0}";
    la = "\\label{$0}";
    bb = "\\begin{${2:$1}}\n$0\n\\end{$2}";
    it = "\\begin{itemize}\n\\item $0\n\\end{itemize}";
    enum = "\\begin{enumerate}\n\\item $0\n\\end{enumerate}";
    use = "\\usepackage{$0}";
    fr = "\\begin{frame}{$0}\n\n\\end{frame}";
    mp2 = [[
    \begin{minipage}{0.45\textwidth}
    \end{minipage}\hfill%
    \begin{minipage}{0.45\textwidth}
    \end{minipage}%
    ]];
    subf = [[
    \begin{figure}
    \centering
    \begin{subfigure}[b]{0.3\textwidth}
    \centering
    \includegraphics[width=\textwidth]{}
    \end{subfigure}
    \hfill
    \begin{subfigure}[b]{0.3\textwidth}
    \centering
    \includegraphics[width=\textwidth]{}
    \end{subfigure}
    \hfill
    \begin{subfigure}[b]{0.3\textwidth}
    \centering
    \includegraphics[width=\textwidth]{}
    \end{subfigure}
    \caption{Of all images}
    \label{fig:all}
    \end{figure}
    ]];
    fig = [[
    \begin{figure}
    \includegraphics[width=\textwidth]{$0}
    \end{figure}
    ]];
    g = "\\gls{$0}";
    gp = "\\glspl{$0}";
    sec = "\\section{$0}";
    i = "\\gls{imu}";
    is = "\\glspl{imu}";
    ti = "\\textit{$0}";
    tb = "\\textit{$0}";
}

local plaintex = v.copy(tex)

local rust = {
    ddbg = "#[derive(Debug)]";
    pn = [[println!("$0")]];
    ifdbg = "#[cfg(debug_assertions)]";
}

require('snippets').snippets = {
    _global = {
        todo = "TODO(hgf): ";
    },
    rust = rust,
    python = python,
    c = c,
    tex = tex,
    plaintex = plaintex,
    cpp = cpp,
}
