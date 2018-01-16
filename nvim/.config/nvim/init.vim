" TODO:
" * C environment
"   - Quickly switch to header of source file
"   - Automatically update function declarations in header (LSP?)
" vim: foldmethod=marker foldlevelstart=0:
if(has('nvim'))
	source ~/.config/nvim/packages.vim
endif

" Plugin Settings {{{

" Colorizer - useful, but damn slow
let g:colorizer_startup = 0

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#complete_method = "complete"
let g:deoplete#disable_auto_complete = 1
let g:deoplete#enable_smart_case = 1

" Ale
let g:ale_linters = {
			\'asm': ['gcc -mcpu=cortex-a7'],
			\'c'  : ['gcc -std=gnu99'],
			\}

" Neosnippets
let g:neosnippet#snippets_directory=$HOME.'/.config/nvim/snips/'
let g:neosnippet#disable_runtime_snippets = {
			\   '_' : 1,
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
set incsearch       " do incremental searching
set scrolloff=4     " scroll page once 4 lines from top/bottom
set relativenumber  " use relative numbers
set backspace=2
set hidden          " allow closing unsaved buffers
set path+=**        " useful for using :find
set wildmenu        " Display all matching files when tab completing
set encoding=utf-8  " Sanity
set noequalalways   " Don't resize automatically after closing, opening
set inccommand=nosplit
set clipboard+=unnamedplus
" Show some whitespace
set listchars=tab:»\ ,trail:-,nbsp:+
set list
" Tab settings
let indent_len=8
let &tabstop=8
let &shiftwidth=indent_len
let &softtabstop=indent_len

set undofile
set swapfile
set gdefault

let g:tex_flavor='latex'

" Colourscheme
set termguicolors
set background=light
colorscheme lucius

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

" General Mappings {{{
"set leader key to:
let mapleader = "\<Space>"
let maplocalleader = ","

" Completion options {{{
set completeopt=menu,preview,longest,noselect
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Smart tab
function! CleverTab()
	if pumvisible()
		return "\<C-N>"
	endif
	if strpart( getline('.'), 0, col('.')-1 ) =~ '^\s*$'
		return "\<Tab>"
	else
		return "\<C-N>"
	endif
endfunction
inoremap <Tab> <C-R>=CleverTab()<CR>
" }}}

" TODO: Remove this absurd crap
function! MnemonicMap(arg,...)
	let s:str = "noremap <leader>" . a:arg
	" a[0] mode
	" a[1] key
	" a[2] value/command
	" a[3] description
	for a in a:000
		let s:mode = a[0]
		let s:key = a[1]
		let s:value = a[2]
		execute s:mode . s:str . s:key . " " . s:value
	endfor
endfunction

" Files
call MnemonicMap('f',
			\['n', 's', ':w<cr>', 'Save buffer'],
			\['n', 'f', ':find ', 'Find file'],
			\['n', 've', ':edit $MYVIMRC<cr>', 'Edit vimrc'],
			\['n', 'vs', ':source $MYVIMRC<cr>', 'Source vimrc'],
			\['n', 'y', ':set ft=', 'Set filetype'],
			\['n', 'x', ':set syntax=', 'Set syntax'],
			\)

" Windows
call MnemonicMap('w',
			\['n', 'h', '<c-w>h'],
			\['n', 'j', '<c-w>j'],
			\['n', 'k', '<c-w>k'],
			\['n', 'l', '<c-w>l'],
			\['n', 's', '<c-w>s'],
			\['n', 'v', '<c-w>v'],
			\['n', 'd', '<c-w>q'],
			\)

" Tabs
call MnemonicMap('t',
			\['n', 'j', 'gt'],
			\['n', 'k', 'gT'],
			\['n', 't', ':tabnew<cr>'],
			\['n', 'd', ':tabclose<cr>'],
			\)

" Git
call MnemonicMap('g',
			\['n', 's', ':Gstatus<cr>'],
			\)
" Buffers
call MnemonicMap('b',
			\['n', 'b', ':Denite buffer<cr>'],
			\['n', 'n', ':enew<cr>'],
			\)

" Quit
call MnemonicMap('q',
			\['n', 'q', ':xall<cr>'],
			\)

" Normal mode mappings {{{
" Save file

" Navigation
nnoremap j gj
nnoremap k gk
nnoremap <leader><tab> :b#<cr>
nnoremap ; :
nnoremap : ;
" Don't mess up reg when using x
nnoremap x "_x
" Strong left
nnoremap H ^
" Strong right
nnoremap L $
" Jump to line number with Enter
nnoremap <cr> G
" Jump to beginning with backspace
nnoremap <Backspace> gg
" Quick terminal, 15 rows tall
nnoremap <leader>te <c-w>s<c-w>j15<c-w>_:te<cr>
" OS clipboard
nnoremap <leader>y "+y
nnoremap <leader>p "+p
nnoremap <leader>P o<c-r>+<ESC>==
" Turn off highlighting
nnoremap <silent> <c-l> :noh<cr>
" Add empty line after cursor
nnoremap <leader>o o<Esc>k
" Add empty line before cursor
nnoremap <leader>O O<Esc>j
" Open file in Emacs with same cursor position
" nnoremap <leader>m :w<cr>:call EmacsEdit()<cr>
" Run make
nnoremap <leader>mm :make<cr>
" Switch to last buffer
" Session vim stuff
nnoremap <leader>ss :SaveSession<cr>
nnoremap <leader>so :OpenSession<space>
nnoremap <leader>sq :CloseTabSession<cr><C-w>q
nnoremap <leader>sQ :CloseSession<cr>
nnoremap <leader>sd :OpenSession code<cr>
nnoremap <leader>std :AppendTabSession! tabcode<cr>
" Snippets
nnoremap <leader>es :NeoSnippetEdit -split<cr>
" Indentation
nnoremap <leader>= mz=ip`z
" }}}

" Visual mode mappings {{{
" Repeat last command on all selected lines
vnoremap . :norm.<CR>
vnoremap : ;
vnoremap ; :
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
imap <C-e> <Plug>(neosnippet_expand_or_jump)
smap <C-e> <Plug>(neosnippet_expand_or_jump)
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
" Open the current file in emacs
function! EmacsEdit ()
	execute 'te emacs +' . line('.') . ':' . col('.') . ' ' . '%'
endfunction

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

" Terminal colours {{{
" lucius colors
let g:terminal_color_0 = '#444444'
let g:terminal_color_1 = '#af0000'
let g:terminal_color_2 = '#008700'
let g:terminal_color_3 = '#af5f00'
let g:terminal_color_4 = '#005faf'
let g:terminal_color_5 = '#870087'
let g:terminal_color_6 = '#008787'
let g:terminal_color_7 = '#eeeeee'
let g:terminal_color_8 = '#444444'
let g:terminal_color_9 = '#af0000'
let g:terminal_color_10 = '#008700'
let g:terminal_color_11 = '#af5f00'
let g:terminal_color_12 = '#005faf'
let g:terminal_color_13 = '#870087'
let g:terminal_color_14 = '#008787'
let g:terminal_color_15 = '#eeeeee'
" }}}

" Neovim specific {{{
if has('nvim') && executable('nvr')
	let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif
" }}}
