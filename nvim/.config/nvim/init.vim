" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
" vim: foldmethod=marker foldlevelstart=0:

" Plugin Settings {{{

call plug#begin(stdpath('data') . '/plugged')

" Completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Filetypes

Plug 'elixir-editors/vim-elixir'
Plug 'kballard/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'sgur/vim-editorconfig'
Plug 'shougo/neosnippet.vim'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'fatih/vim-go'
Plug 'ledger/vim-ledger'

" Themes
Plug 'arcticicestudio/nord-vim'
Plug 'sjl/badwolf'
Plug 'dracula/vim', {'as': 'dracula.vim'}
call plug#end()

let g:pandoc#syntax#conceal#use = 0
let s:nvim_config = $HOME.'/.config/nvim'

" Neosnippets
let g:neosnippet#disable_select_mode_mappings = 1
let g:neosnippet#snippets_directory= s:nvim_config . '/snips/'
let g:neosnippet#disable_runtime_snippets = {
      \   '_': 1,
      \ }

" }}}

" Basic Settings {{{

filetype plugin indent on
syntax enable

set ignorecase      " ignores case when searching
set smartcase       " case-sensitive when using capital in search
" set number
set history=1000
set scrolloff=4     " scroll page once 4 lines from top/bottom
set backspace=2
set hidden          " allow closing unsaved buffers
set path=**         " useful for using :find et al.
set wildmenu        " Display all matching files when tab completing
set wildignorecase  " Wild menu ignores case
set encoding=utf-8
set noequalalways   " Don't resize automatically after closing, opening
set inccommand=nosplit
set nowrap
set textwidth=120
" Show some whitespace
set listchars=tab:\ \ ,trail:·,nbsp:+
set list
" Tab settings
set tabstop=8
set shiftwidth=8
set softtabstop=8

" Backup
set nobackup
set nowritebackup

set updatetime=300

set cmdheight=2

set shortmess+=c

set signcolumn=yes

" completion
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"


set undofile
set swapfile
set gdefault
set number

inoremap <silent><expr> <c-space> coc#refresh()

" Grepping {{{

function! BuildGrepExclude()
  let s:exclude = ""
  if filereadable('.gitignore')
          let s:exclude = s:exclude . " --exclude-from='.gitignore'"
  endif
  let s:global_gitignore = $HOME . "/.config/git/ignore"
  if filereadable(s:global_gitignore)
    let s:exclude = s:exclude . " --exclude-from=" . s:global_gitignore
  endif
  return s:exclude
endfunction

let g:grepprg_string="grep -n " . BuildGrepExclude() . " $*"
let &grepprg=g:grepprg_string

command! -nargs=+ Grep execute 'silent grep! -r <args>' | copen 10

" }}}

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
set noruler           " hide ruler to always use glorious statusline
set laststatus=2      " always show statusline
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
set completeopt=menu,preview,longest
set omnifunc=lsp#omnifunc

" This is optional, but you may find it useful
autocmd CompleteDone * pclose

" Navigation
nnoremap j gj
nnoremap k gk
nnoremap <CR> G
" Strong left
nnoremap H ^
" Strong right
nnoremap L $
" Saving
nnoremap <C-s> :w<cr>

" vimrc
nnoremap <leader>fe :edit $MYVIMRC<cr>
nnoremap <leader>fs :source $MYVIMRC<cr>

" OS clipboard
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>P o<c-r>+<ESC>==
" Snippets
nnoremap <leader>es :NeoSnippetEdit -split<cr>
" Don't mess up reg when using x
nnoremap x "_x
" Turn off highlighting
nnoremap <silent> <c-l> :noh<cr>
" Add empty line after cursor
nnoremap <leader>o o<Esc>k
" Add empty line before cursor
nnoremap <leader>O O<Esc>j
" making things
nnoremap <space>m :!make<cr>
nnoremap <space>c :!make clean<cr>
" Indentation
nnoremap <leader>= mz=ip`z

" Commenting
nnoremap <M-;> :<C-u>Commentary<cr>

" Visual {{{
" Repeat last command on all selected lines
vnoremap . :norm.<CR>
" Don't replace "" when pasting in visual
vnoremap p "_c<Esc>p
vmap <leader>e <Plug>(neosnippet_expand_target)

vnoremap <M-;> :Commentary<cr>

" }}}

" Insert mode mappings {{{
" I'm stupid and <C-i> is actually the same as Tab
" I'm stupid and <C-j> is used a few lines below for neosnippet
" Insert today
inoremap <C-k><C-t> <C-r>=strftime("%Y-%m-%d")<cr>

" Snippets
imap <C-j> <Plug>(neosnippet_expand_or_jump)
smap <C-j> <Plug>(neosnippet_expand_or_jump)
" }}}

" Terminal mode mappings {{{
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
function! SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

function! SynGroup()
  let l:s = synID(line('.'), col('.'), 1)
  echo synIDattr(l:s, 'name') . ' -> ' . synIDattr(synIDtrans(l:s), 'name')
endfunction

function! YankUnityTestGroupName()
  let l:line = search("TEST_GROUP", "bn")
  let l:line_txt = getline(l:line)
  let l:test_name = substitute(l:line_txt, "^.*(\\(.*\\))$", "\\1", "")
  return l:test_name
endfunction

nnoremap <space>h :<c-u>call SynGroup()<cr>
" }}}

" Rust {{{
let g:rustfmt_autosave = 1
" }}}
