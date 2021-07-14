(import-macros {
                : call
                } :fnl.macros)

(local packer (require :packer))

(macro use! [name ...]
       (let [cnt (select "#" ...)
             tbl {1 (view name)}]
         (for [i 1 cnt 2]
           (tset tbl (select i ...) (select (+ 1 i) ...)))
       `(,(sym "use") ,tbl)))

(packer.startup
 (fn []
  (use! wbthomason/packer.nvim)

  ;; themes
  (use! shaunsingh/nord.nvim
        :config #(call :nord set))

  ;; the classics
  (use! tpope/vim-surround)
  (use! tpope/vim-repeat)
  (use! tpope/vim-rsi)
  (use! tpope/vim-fugitive)
  (use! tommcdo/vim-exchange)

  ;; ~Magit~ Neogit
  (use! TimUntersberger/neogit
        :requires ["nvim-lua/plenary.nvim"]
        :config #(call :neogit setup))

  ;; Fennel!
  (use! bakpakin/fennel.vim)

  ;; fancy git signs
  (use! lewis6991/gitsigns.nvim
        :requires ["nvim-lua/plenary.nvim"]
        :config #(call :gitsigns setup))

  ;; fuzzy
  (use! nvim-telescope/telescope.nvim
        :requires ["nvim-lua/popup.nvim" "nvim-lua/plenary.nvim"])

  ; lyaml also requires system libyaml-dev
  (use! norcalli/snippets.nvim
        :config #(require :config.snippets)
        :rocks ["lyaml"])
 ))

