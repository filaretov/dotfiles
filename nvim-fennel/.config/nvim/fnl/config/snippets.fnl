(import-macros {: key : on : augroup} :fnl.macros)

(local fun (require :functions))
(local yaml (require :lyaml))
(local snippets-path (fun.config "snips.yml"))

(key i "<c-j>" "<cmd>lua return require'snippets'.expand_or_advance(1)<cr>")
(key i "<c-k>" "<cmd>lua return require'snippets'.advance_snippet(-1)<CR>")
(key n "gs" "<cmd>lua vim.cmd('edit '..vim.fn.stdpath('config')..'/snips.yml')<cr>")

(fn set-snippets []
  (with-open [fin (io.open snippets-path)]
    (let [snippets (yaml.load (fin:read "*all"))]
      (tset (require :snippets) :snippets snippets))))

(augroup snippets
         (on [BufWritePost] "snips.yml" (set-snippets)))

(set-snippets)
