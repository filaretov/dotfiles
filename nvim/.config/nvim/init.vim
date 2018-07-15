" vim: foldmethod=marker foldlevelstart=0:

" Plugins {{{

command! PackUpdate packadd minpac | source $MYVIMRC | redraw | call minpac#update()
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()

if exists('*minpac#init')
  call minpac#init()
  call minpac#add('justinmk/vim-dirvish')
  call minpac#add('kballard/vim-fish')
  call minpac#add('k-takata/minpac', {'type': 'opt'})
  call minpac#add('lervag/vimtex')
  call minpac#add('romainl/flattened')
  call minpac#add('rust-lang/rust.vim')
  call minpac#add('sgur/vim-editorconfig')
  call minpac#add('Shougo/neosnippet.vim')
  call minpac#add('tommcdo/vim-exchange')
  call minpac#add('tpope/vim-abolish')
  call minpac#add('tpope/vim-commentary')
  call minpac#add('tpope/vim-eunuch')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('tpope/vim-obsession')
  call minpac#add('tpope/vim-repeat')
  call minpac#add('tpope/vim-rsi')
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')
  call minpac#add('filaretov/vim-todo')
  call minpac#add('filaretov/vim-orgzly')
  call minpac#add('romainl/flattened')
  call minpac#add('vim-pandoc/vim-pandoc-syntax')
  call minpac#add('roxma/nvim-yarp')
  call minpac#add('ncm2/ncm2')
  call minpac#add('ncm2/ncm2-bufword')
  call minpac#add('ncm2/ncm2-path')
  call minpac#add('ncm2/ncm2-jedi')
  call minpac#add('filipekiss/ncm2-look.vim')
  call minpac#add('dracula/vim', {'name' : 'vim-dracula'})
endif

" }}}

" Plugin Settings {{{

let g:pandoc#syntax#conceal#use = 0
let s:nvim_config = $HOME.'/.config/nvim'

" Obsession

function! SessionPath()
  let l:project_dir = substitute(getcwd(), $HOME.'/', "", "")
  let l:project_name = substitute(l:project_dir, "/", '\\%', "g")
  let l:obsession_path = s:nvim_config . '/sessions/' . l:project_name . '.vim'
  return l:obsession_path
endfunction

function! SessionStart()
  exec "Obsession " . SessionPath()
endfunction

function! SessionLoad()
  exec "silent! source " . SessionPath()
endfunction

command! SessionStart call SessionStart()
command! SessionLoad call SessionLoad()

" Neosnippets
let g:neosnippet#disable_select_mode_mappings=1
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
set number
set history=1000
set scrolloff=4     " scroll page once 4 lines from top/bottom
set backspace=2
set hidden          " allow closing unsaved buffers
set path+=**        " useful for using :find et al.
set wildmenu        " Display all matching files when tab completing
set wildignorecase  " Wild menu ignores case
set encoding=utf-8
set noequalalways   " Don't resize automatically after closing, opening
set inccommand=nosplit
set nowrap
" Show some whitespace
set listchars=tab:\ \ ,trail:·,nbsp:+
set list
" Tab settings
set tabstop=8
set shiftwidth=8
set softtabstop=8

set undofile
set swapfile
set gdefault

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

" Python neovim path
let g:python3_host_prog = $HOME.'/.miniconda3/envs/neovim/bin/python'
let g:python_host_prog = '/usr/bin/python2'

" Colourscheme
set termguicolors
colorscheme flattened_light

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

" Completion options {{{
set completeopt=noinsert,menuone,noselect
" }}}

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
nnoremap <leader>ev :edit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

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
" making things
nnoremap <space>m :!make<cr>
nnoremap <space>c :!make clean<cr>
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
" I'm stupid and <C-i> is actually the same as Tab
" I'm stupid and <C-j> is used a few lines below for neosnippet
" Insert today
inoremap <C-k><C-t> <C-r>=strftime("%Y-%m-%d")<cr>

" Snippets
imap <C-j> <Plug>(neosnippet_expand_or_jump)
smap <C-j> <Plug>(neosnippet_expand_or_jump)
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

augroup terminal
  autocmd!
  autocmd TermOpen * setlocal nonumber
augroup END

augroup pandoc
  au!
  au BufNewFile,BufRead README set filetype=pandoc
  au BufNewFile,BufRead *.markdown,*.mkd,*.md set filetype=pandoc
augroup END

augroup ncm2
  au!
  au BufEnter * call ncm2#enable_for_buffer()
  au User Ncm2Plugin call ncm2#register_source({
          \ 'name' : 'vimtex',
          \ 'priority': 9,
          \ 'subscope_enable': 1,
          \ 'complete_length': 1,
          \ 'scope': ['tex'],
          \ 'mark': 'tex',
          \ 'word_pattern': '\w+',
          \ 'complete_pattern': g:vimtex#re#ncm,
          \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
          \ })
augroup END

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

nnoremap <space>h :<c-u>call SynGroup()<cr>
" }}}
