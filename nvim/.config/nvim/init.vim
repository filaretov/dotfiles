" vim: foldmethod=marker foldlevelstart=0:
" TODO:
" * C environment
"   - Quickly switch to header of source file
"   - Automatically update function declarations in header (LSP?)

" Plugins {{{

command! PackUpdate packadd minpac | source $MYVIMRC | redraw | call minpac#update()
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()

if exists('*minpac#init')

	call minpac#init()
	call minpac#add('k-takata/minpac', {'type': 'opt'})
	call minpac#add('kballard/vim-fish')
	call minpac#add('rust-lang/rust.vim')
	call minpac#add('Shougo/neosnippet.vim')
	call minpac#add('tommcdo/vim-exchange')
	call minpac#add('tpope/vim-repeat')
	call minpac#add('tpope/vim-eunuch')
	call minpac#add('tpope/vim-surround')
	call minpac#add('tpope/vim-vinegar')
	call minpac#add('tpope/vim-rsi')
	call minpac#add('tpope/vim-unimpaired')
	call minpac#add('tpope/vim-obsession')
	call minpac#add('tpope/vim-speeddating')
	call minpac#add('tpope/vim-abolish')
	call minpac#add('justinmk/vim-sneak')
	call minpac#add('vim-pandoc/vim-pandoc')
	call minpac#add('vim-pandoc/vim-pandoc-syntax')
	call minpac#add('w0rp/ale')
	call minpac#add('arcticicestudio/nord-vim')
	call minpac#add('autozimu/LanguageClient-neovim', {
				\'do' : {-> system('bash install.sh')},
				\'branch' : 'next'
				\})
	call minpac#add('roxma/nvim-completion-manager')
endif

" }}}

" Plugin Settings {{{

" LanguageClient

let g:LanguageClient_serverCommands = {
			\ 'cpp': ['cquery', '--log-file=/tmp/cq.log'],
			\ 'c': ['cquery', '--log-file=/tmp/cq.log'],
			\ 'python': ['pyls', '-v', '--log-file=/tmp/pyls.log']
			\}

let g:LanguageClient_loadSettings = 1
let g:LanguageClient_settingsPath = $HOME.'/.config/nvim/settings.json'
set completefunc=LanguageClient#complete
set formatexpr=LanguageClient_textDocument_rangeFormatting()
nnoremap <silent> gR :call LanguageClient_textDocument_rename()<CR>
nnoremap <silent> gr :call LanguageClient_textDocument_references()<CR>
nnoremap <silent> gh :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>

" Ale
let g:ale_linters = {
			\'asm': ['gcc -mcpu=cortex-a7'],
			\'c': ['gcc -std=gnu99'],
			\}

" Neosnippets
let g:neosnippet#snippets_directory=$HOME.'/.config/nvim/snips/'
let g:neosnippet#disable_runtime_snippets = {
			\   '_': 1,
			\ }

" Pandoc
let g:pandoc#folding#level=0

" }}}

" Basic Settings {{{

filetype plugin indent on " use filetype settings
syntax enable " syntax highlighting

set ignorecase      " ignores case when searching
set smartcase       " case-sensitive when using capital in search
set number          " show linenumber
set history=1000
set scrolloff=4     " scroll page once 4 lines from top/bottom
set relativenumber  " use relative numbers
set backspace=2
set hidden          " allow closing unsaved buffers
set path+=**        " useful for using :find
set wildmenu        " Display all matching files when tab completing
set encoding=utf-8  " Sanity
set noequalalways   " Don't resize automatically after closing, opening
set inccommand=nosplit
" Show some whitespace
set listchars=tab:\|\ ,trail:-,nbsp:+
set list
" Tab settings
set tabstop=8
set shiftwidth=8
set softtabstop=8

set undofile
set swapfile
set gdefault

let g:tex_flavor='latex'

" Colourscheme
set background=dark
colorscheme nord

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
set cino=N-s
" }}}

" Status Line {{{
set noruler           " hide ruler to always use glorious statusline
set laststatus=2      " always show statusline
set statusline=\ 
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

" Completion options {{{
set completeopt=menu,preview,longest,noselect
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" }}}

" Navigation
nnoremap j gj
nnoremap k gk
nnoremap <leader><tab> :b#<cr>
" Strong left
nnoremap H ^
" Strong right
nnoremap L $
" Jump to line number with Enter
nnoremap <cr> G
" Jump to beginning with backspace
nnoremap <Backspace> gg

" vimrc
nnoremap <leader>ev :edit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>
nnoremap <leader>ep :edit $MYVIMRC<cr>

" Quicksave
nnoremap <leader>w :w<cr>

" OS clipboard
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>P o<c-r>+<ESC>==
" Snippets
nnoremap <leader>es :NeoSnippetEdit -split<cr>
" Quick terminal, 15 rows tall
nnoremap <leader>t <c-w>s<c-w>j15<c-w>_:te<cr>
" Don't mess up reg when using x
nnoremap x "_x
" Turn off highlighting
nnoremap <silent> <c-l> :noh<cr>
" Add empty line after cursor
nnoremap <leader>o o<Esc>k
" Add empty line before cursor
nnoremap <leader>O O<Esc>j
" Run make
nnoremap <leader>m :make<cr>
" Switch to last buffer
" Indentation
nnoremap <leader>= mz=ip`z

" Visual {{{
" Repeat last command on all selected lines
vnoremap . :norm.<CR>
" Don't replace "" when pasting in visual
vnoremap p "_c<Esc>p
vmap <leader>e <Plug>(neosnippet_expand_target)

" }}}

" Insert mode mappings {{{
inoremap <M-h> <ESC><C-W>h
inoremap <M-j> <ESC><C-W>j
inoremap <M-k> <ESC><C-W>k
inoremap <M-l> <ESC><C-W>l
" Umlaut
inoremap <C-f>o ö
inoremap <C-f>u ü
inoremap <C-f>a ä
inoremap <C-f>s ß
" Newlines
inoremap <C-l> <ESC>o
inoremap <C-j> <down>

" Snippets
imap <C-i> <Plug>(neosnippet_expand_or_jump)
smap <C-i> <Plug>(neosnippet_expand_or_jump)
" }}}

" Terminal mode mappings {{{
" Escape mode
tnoremap <ESC> <C-\><C-n>
tnoremap <M-h> <C-\><C-n><C-w>h
tnoremap <M-j> <C-\><C-n><C-w>j
tnoremap <M-k> <C-\><C-n><C-w>k
tnoremap <M-l> <C-\><C-n><C-w>l
tnoremap <M-q> <C-\><C-n><C-w>q
" }}}
" }}}

" Commands and Functions {{{
" Silences command and redraws screen
command! -nargs=1 Silent
			\ | execute ':silent !'.<q-args>
			\ | execute ':redraw!'
" }}}

" Text FileType Settings {{{ augroup filetype_txt
augroup filetype_text
	autocmd!
	" For all text files set 'textwidth' to 78 characters.
	autocmd FileType text setlocal textwidth=78
augroup end
" }}}

" Filetype augroups {{{
augroup c_headers
	autocmd!
	autocmd BufNewFile,BufRead *.h set filetype=c
augroup end

augroup filetype_term
	autocmd!
	autocmd BufEnter term://* startinsert
augroup end

"Include md as markdown file extension
augroup md_extensions
	autocmd!
	autocmd BufNewFile,BufRead *.md set filetype=markdown
augroup end

" Wiki filetype
augroup wiki_ext
	autocmd!
	autocmd BufNewFile,BufRead *.wiki set filetype=pandoc
	autocmd BufNewFile,BufRead *.page set filetype=pandoc
augroup end
" }}}

" Neovim specific {{{
if has('nvim') && executable('nvr')
	let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif
" }}}
