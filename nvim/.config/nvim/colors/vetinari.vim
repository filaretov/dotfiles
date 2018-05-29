" Vim color file
" Maintainer: Hristo Filaretov <h.filaretov@protonmail.com>
" Si non confectus, non reficiat

set background=dark
highlight clear
if exists("syntax_on")
  syntax reset
endif

"colorscheme default
let g:colors_name = "vetinari"

"         GUI
highlight Normal          guifg=#424242     guibg=#f1f1f1  gui=NONE
highlight Search          guifg=#424242     guibg=#ffff88
highlight IncSearch       guifg=#424242     guibg=#ffff88  gui=NONE
highlight Visual          guibg=#a0d0f0
highlight Cursor          guifg=#0000ff     guibg=#424242
highlight Comment         guifg=#aaaaaa     guibg=#f1f1f1 gui=italic
highlight NonText         guifg=#aaaaaa     guibg=#f1f1f1 gui=NONE
highlight Whitespace      guifg=#aaaaaa     guibg=#f1f1f1 gui=NONE
highlight Statement       guifg=#424242     guibg=#f1f1f1 gui=bold
highlight Identifier      guifg=#424242     guibg=#f1f1f1 gui=bold
highlight Bold            guifg=#424242     guibg=#f1f1f1 gui=bold
highlight! link Title    Bold
highlight! link htmlBold Bold

" Blue
highlight PreProc         guifg=#2060a0     guibg=#f1f1f1
highlight Constant        guifg=#2060a0     guibg=#f1f1f1
highlight SpecialKey      guifg=#2060a0     guibg=#f1f1f1
highlight Directory       guifg=#2060a0     guibg=#f1f1f1
highlight MatchParen      guifg=#2060a0     guibg=#f1f1f1 gui=bold
highlight Underlined      guifg=#2060a0     guibg=#f1f1f1 gui=underline,NONE

" Green
highlight Todo            guifg=#20a060     guibg=#f1f1f1 gui=italic
highlight Type            guifg=#20a060     guibg=#f1f1f1 gui=NONE

" Red
highlight ErrorMsg        guifg=#e04040     guibg=#f1f1f1 gui=reverse

" Yellow
highlight Special         guifg=#c0a040     guibg=#f1f1f1

" Magenta

" Interface
highlight LineNr          guifg=#b0b0b0     guibg=#f1f1f1
highlight CursorLineNr    guifg=#b0b0b0     guibg=#f1f1f1
highlight Pmenu           guifg=#f1f1f1     guibg=#424242
highlight Folded          guifg=#424242     guibg=#f1f1f1  gui=bold
highlight StatusLine      guifg=#424242     guibg=#f1f1f1  gui=bold,underline
highlight StatusLineNC    guifg=#a0a0a0     guibg=#f1f1f1  gui=underline
highlight VertSplit       guifg=#424242     guibg=#f1f1f1  gui=NONE
highlight WildMenu        guifg=#424242     guibg=#ffff88  gui=bold
highlight SignColumn      guifg=#424242     guibg=#f1f1f1  gui=bold

" Neovim
let g:terminal_color_0 = "#424242"
let g:terminal_color_1 = "#e04040"
let g:terminal_color_2 = "#20a060"
let g:terminal_color_3 = "#f0d060"
let g:terminal_color_4 = "#2060a0"
let g:terminal_color_5 = "#a060a0"
let g:terminal_color_6 = "#20c0c0"
let g:terminal_color_7 = "#f1f1f1"

"         Console
highlight Normal        ctermfg=LightGrey ctermbg=Black
highlight Search        ctermfg=Black     ctermbg=Red    cterm=NONE
highlight Visual        cterm=reverse
highlight Cursor        ctermfg=Black     ctermbg=Green
highlight Special       ctermfg=Brown
highlight Comment       ctermfg=Blue
highlight StatusLine    ctermfg=blue      ctermbg=white
highlight Statement     ctermfg=White
highlight Type          cterm=NONE
