(import-macros {: opt
                : bopt
                : wopt
                : key
                : on
                : augroup
                : cmd
                : call
                } :fnl.macros)

(local fun (require :functions))

(require :plugins)

(tset _G :dump fun.dump)

(opt hidden)
(opt termguicolors)
(opt ignorecase)
(opt smartcase)
(opt inccommand :nosplit)
(opt gdefault)

(bopt expandtab true)
(bopt shiftwidth 4)
(bopt tabstop 4)
(bopt smartindent)

(wopt number true)
(wopt wrap false)

; Easily compile and load new fnl code
(key n "gm" (cmd (fun.make-fennel)))
(key n "ge" (cmd (fun.telescope-find-here)))

; Blink on yank
(on [TextYankPost] "*" (vim.highlight.on_yank {:on_visual false}))

; Key maps
(key n "s" "<C-w>")
(key n "st" "<cmd>terminal<cr>i")
(key n "gcc" (cmd (fun.edit (fun.config "fnl/startup.fnl"))))
(key n "gcp" (cmd (fun.edit (fun.config "fnl/plugins.fnl"))))
(key [n i] "<c-s>" (cmd (fun.save-buffer)))
(key n "<c-p>" (cmd (call :telescope.builtin find_files)))
(key n "gp" "\"+p")
(key n "L" (cmd (fun.end-of-line)))
(key n "H" (cmd (fun.beginning-of-line)))
(key n "<space>g" (cmd (vim.cmd "Neogit")))

; Terminal
(key t "<esc>" "<C-\\><C-n>")

(augroup terminal
         (on [TermOpen] "*" (vim.cmd "setlocal nonumber"))
         (on [TermOpen] "*" (vim.cmd "setlocal matchpairs="))
         (on [TermOpen] "*" (vim.cmd "setlocal scrolloff=0")))
