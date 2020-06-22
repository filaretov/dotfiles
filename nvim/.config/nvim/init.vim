" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
" vim: foldmethod=marker foldlevelstart=0:

" Helper values {{{
let s:nvim_config = $HOME.'/.config/nvim'
" }}}

" Plug {{{
call plug#begin(stdpath('data') . '/plugged')

" Completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Filetypes
Plug 'rust-lang/rust.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'kballard/vim-fish'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'fatih/vim-go'
Plug 'ledger/vim-ledger'
Plug 'cespare/vim-toml'

" Goodies
Plug 'sgur/vim-editorconfig'
Plug 'shougo/neosnippet.vim'
Plug 'tommcdo/vim-exchange'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
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
let g:pandoc#syntax#conceal#use = 0

" Neosnippet
let g:neosnippet#disable_select_mode_mappings = 1
let g:neosnippet#snippets_directory= s:nvim_config . '/snips/'
let g:neosnippet#disable_runtime_snippets = {
      \   '_': 1,
      \ }

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
" Saving
nnoremap <leader>w :w<cr>
nnoremap <C-s> :<C-u>w<cr>

" Last buffer
nnoremap <leader><tab> :b#<cr>

" Quickly opening files {{{
nnoremap <C-p> :<C-u>FZF<cr>
nnoremap <leader>fe :<C-u>edit $MYVIMRC<cr>
nnoremap <leader>fs :<C-u>NeoSnippetEdit -split<cr>
nnoremap <leader>fn :<C-u>edit ~/cloud/journal/notes.md<cr>
nnoremap <leader>fu :<C-u>edit ~/cloud/journal/uni.md<cr>
nnoremap <leader>fw :<C-u>edit ~/cloud/journal/wiki.md<cr>
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

nnoremap <C-l> zz

" Visual {{{
" Repeat last command on all selected lines
vnoremap . :norm.<CR>
" Don't replace "" when pasting in visual
vnoremap p "_c<Esc>p


" }}}

" Insert mode mappings {{{
" I'm stupid and <C-i> is actually the same as Tab
" I'm stupid and <C-j> is used a few lines below for neosnippet

" Snippets
imap <C-j> <Plug>(neosnippet_expand_or_jump)
smap <C-j> <Plug>(neosnippet_expand_or_jump)
" }}}

" Terminal mode mappings {{{
nnoremap <leader>t :<C-u>te<cr>
tnoremap <ESC> <C-\><C-n>
" }}}
" }}}

" Filetype augroups {{{
augroup c_headers
  autocmd!
  autocmd BufNewFile,BufRead *.h set filetype=c
augroup end

"Include md as markdown file extension
augroup md_extensions
  autocmd!
  autocmd BufNewFile,BufRead *.md set filetype=markdown
augroup end

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

" }}}

" Rust {{{
let g:rustfmt_autosave = 1
" }}}

" GNVim {{{
if exists('g:gnvim')
  source $HOME/.config/nvim/ginit.vim
end
" }}}

" Triage {{{
let g:markdown_folding = 1

set foldtext=MyFoldText()
function! MyFoldText()
  let line = getline(v:foldstart)
  let sub = substitute(line, '{{{', '', 'g')
  return sub . "..."
endfunction
" }}}
" }}}

" Highlight Patches {{{
highlight! link Folded Bold
highlight! clear Conceal
" }}}
