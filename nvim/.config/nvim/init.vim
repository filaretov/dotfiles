" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
" vim: foldmethod=marker foldlevelstart=0:

" Plug {{{
call plug#begin(stdpath('data') . '/plugged')

" Completion
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neovim/nvim-lsp'

" Filetypes
Plug 'rust-lang/rust.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'kballard/vim-fish'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'fatih/vim-go'
Plug 'ledger/vim-ledger'
Plug 'cespare/vim-toml'
Plug 'jceb/vim-orgmode'

" Goodies
Plug 'sgur/vim-editorconfig'
Plug 'shougo/neosnippet.vim'
Plug 'tommcdo/vim-exchange'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" Themes
Plug 'cocopon/inspecthi.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'sjl/badwolf'
Plug 'dracula/vim', {'as': 'dracula.vim'}
call plug#end()
" }}}

" Plugin Settings {{{

" pandoc {{{
let g:pandoc#syntax#conceal#use = 0
" }}}

" Neosnippet {{{
let g:neosnippet#disable_select_mode_mappings = 1
let g:neosnippet#snippets_directory= $HOME.'/.config/nvim/snips/'
let g:neosnippet#disable_runtime_snippets = {
      \   '_': 1,
      \ }
" }}}

" }}}

" Basic Settings {{{

set hidden
set ignorecase
set smartcase
set inccommand=nosplit
set nowrap
set number
set path=**
set scrolloff=4
set textwidth=90
set wildignorecase
set list
set shell=/usr/bin/fish
set nobackup
set nowritebackup
set shortmess+=c
set fillchars=fold:\ 
set undofile
set swapfile
set gdefault
let g:tex_flavor='latex'

" Colourscheme
set termguicolors
set background=dark
color nord
command! Weatherwax :e ~/.config/nvim/colors/weatherwax.vim
command! Vetinari :e ~/.config/nvim/colors/vetinari.vim

augroup vimrcEx
  au!
  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
        \ if line("'\"") >= 1 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
augroup end

" }}}

" Indentation settings {{{
" Don't indent namespaces
set cino=N-s,i0,g0
" }}}

" Status Line {{{
set statusline=
set statusline+=%t
set statusline+=\ %y
set statusline+=\ %(\ [%M%R]%)
" }}}

" GUI Cursor {{{
set guicursor=
set guicursor+=a:blinkon0
set guicursor+=n-v:block
set guicursor+=i-c:ver1
" }}}

" Mappings {{{
"set leader key to:
let mapleader = "\<Space>"
let maplocalleader = ","

" Navigation
nnoremap j gj
nnoremap k gk
" Strong left
nnoremap H ^
" Strong right
nnoremap L $

nnoremap Y y$
" Saving
nnoremap <leader>w :w<cr>
nnoremap <C-s> :<C-u>w<cr>

" Quickly moving through windows
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l
nnoremap <M-s> <C-w>s
nnoremap <M-v> <C-w>v
nnoremap <M-q> <C-w>q
nnoremap <M-o> <C-w>o
nnoremap <space>s <C-w>s

nnoremap <C-q> <C-c>

" Last buffer
nnoremap <leader><tab> :b#<cr>

" Quickly opening files {{{
nnoremap <C-p> :<C-u>FZF<cr>
nnoremap <leader>fe :<C-u>edit $MYVIMRC<cr>
nnoremap <leader>fs :<C-u>NeoSnippetEdit -split<cr>
nnoremap <leader>fn :<C-u>edit ~/cloud/journal/notes.md<cr>
nnoremap <leader>fu :<C-u>edit ~/cloud/journal/uni.md<cr>
nnoremap <leader>fi :<C-u>edit ~/cloud/journal/inbox.md<cr>
nnoremap <leader>ff :<C-u>edit ~/cloud/journal/fraunhofer.md<cr>
" }}}

" OS clipboard {{{
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>P o<c-r>+<ESC>==
" }}}

" Don't mess up reg when using x
nnoremap x "_x
" Turn off highlighting
nnoremap <silent> <c-n> :nohl<cr>
" Commenting
nnoremap <silent> <M-;> :<C-u>Commentary<cr>

" Navigate quickfix list with ease
nnoremap <silent> [q :cprevious<CR>
nnoremap <silent> ]q :cnext<CR>

" Visual {{{
" Repeat last command on all selected lines
vnoremap . :norm.<CR>
" Don't replace " when pasting in visual
vnoremap p "_c<Esc>p
" }}}

" Insert mode mappings {{{
" I'm stupid and <C-i> is actually the same as Tab
" I'm stupid and <C-j> is used a few lines below for neosnippet

" Snippets
imap <C-j> <Plug>(neosnippet_expand_or_jump)
smap <C-j> <Plug>(neosnippet_expand_or_jump)

inoremap <C-k>d <C-R>=strftime("%Y-%m-%d")<cr>
" }}}

" Terminal mode mappings {{{
nnoremap <leader>t :<C-u>te<cr>
tnoremap <ESC> <C-\><C-n>
" }}}

" }}}

" Neovim specific {{{
if has('nvim') && executable('nvr')
  let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif
" }}}

" augroups {{{
augroup terminal
  autocmd!
  autocmd TermOpen * setlocal nonumber
  autocmd TermOpen * setlocal matchpairs=
  autocmd TermOpen * setlocal scrolloff=0
augroup END

augroup pandoc
  au!
  au BufNewFile,BufRead README set filetype=pandoc
  au BufNewFile,BufRead *.markdown,*.mkd,*.md set filetype=pandoc
augroup END

" }}}

" Helper functions {{{
command! Source source $MYVIMRC

command! Time edit ~/cloud/t.timedot

" }}}

" Rust {{{
let g:rustfmt_autosave = 1
" }}}

" Triage {{{

" }}}

" Highlight Patches {{{
highlight! link Folded Comment
highlight! clear Conceal

" }}}

" Tidbits {{{
augroup LuaHighlight
  autocmd!
  autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()
augroup END
" }}}

" LSP {{{
lua <<EOF
    local nvim_lsp = require'nvim_lsp'
    require'nvim_lsp'.vimls.setup{}
    require'nvim_lsp'.pyls.setup{}
    require'nvim_lsp'.rust_analyzer.setup{}
    require'nvim_lsp'.yamlls.setup{}
EOF
autocmd Filetype vim setlocal omnifunc=v:lua.vim.lsp.omnifunc
autocmd Filetype rust setlocal omnifunc=v:lua.vim.lsp.omnifunc
autocmd Filetype python setlocal omnifunc=v:lua.vim.lsp.omnifunc
" }}}

