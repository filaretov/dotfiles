" TODO:
" * C environment
"   - Quickly switch to header of source file
"   - Automatically update function declarations in header (LSP?)
" * Learn fugitive and decide if it's beneficial to workflow
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

" Mappings {{{
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

" Files
nnoremap <space>fs :w<cr>
nnoremap <space>ff :find 
nnoremap <space>fve :edit $MYVIMRC<cr>
nnoremap <space>fvs :source $MYVIMRC<cr>
nnoremap <space>fy :set ft=
nnoremap <space>fx :set syntax=

" Windows
nnoremap <space>wh <c-w>h
nnoremap <space>wj <c-w>j
nnoremap <space>wk <c-w>k
nnoremap <space>wl <c-w>l
nnoremap <space>ws <c-w>s
nnoremap <space>wv <c-w>v
nnoremap <space>wd <c-w>q

nnoremap <m-h> <c-w>h
nnoremap <m-j> <c-w>j
nnoremap <m-k> <c-w>k
nnoremap <m-l> <c-w>l
nnoremap <m-s> <c-w>s
nnoremap <m-v> <c-w>v
nnoremap <m-d> <c-w>q

" Tabs
nnoremap <space>tt :tabnew<cr>
nnoremap <space>td :tabclose<cr>

" Git
nnoremap <space>gs :Gstatus<cr>

" OS clipboard
nnoremap <leader>y "+y
nnoremap <leader>p "+p
vnoremap <leader>y "+y
vnoremap <leader>p "+p
nnoremap <leader>P o<c-r>+<ESC>==
" Snippets
nnoremap <leader>es :NeoSnippetEdit -split<cr>
" Quick terminal, 15 rows tall
nnoremap <leader>te <c-w>s<c-w>j15<c-w>_:te<cr>
" Don't mess up reg when using x
nnoremap x "_x
" Turn off highlighting
nnoremap <silent> <c-l> :noh<cr>
" Add empty line after cursor
nnoremap <leader>o o<Esc>k
" Add empty line before cursor
nnoremap <leader>O O<Esc>j
" Run make
nnoremap <leader>mm :make<cr>
" Switch to last buffer
" Indentation
nnoremap <leader>= mz=ip`z

" Visual
" Repeat last command on all selected lines
vnoremap . :norm.<CR>
" Don't replace "" when pasting in visual
vnoremap p "_c<Esc>p
vmap <leader>e <Plug>(neosnippet_expand_target)

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
