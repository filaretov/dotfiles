" Vim color file
" Maintainer: Hristo Filaretov <h.filaretov@protonmail.com>
" Last Change: 2018-05-27
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
highlight Visual          guibg=#d0e0f0
highlight Cursor          guifg=#0000ff     guibg=#424242
highlight Special         guifg=#424242     guibg=#f1f1f1
highlight Comment         guifg=#aaaaaa     guibg=#f1f1f1 gui=italic
highlight NonText         guifg=#aaaaaa     guibg=#f1f1f1 gui=NONE
highlight Whitespace      guifg=#aaaaaa     guibg=#f1f1f1 gui=NONE
highlight Type            guifg=#424242     guibg=#f1f1f1 gui=NONE
highlight Statement       guifg=#424242     guibg=#f1f1f1 gui=bold
highlight Identifier      guifg=#424242     guibg=#f1f1f1 gui=bold

" Blue keywords
highlight PreProc         guifg=#2040a0     guibg=#f1f1f1
highlight Constant        guifg=#2040a0     guibg=#f1f1f1
highlight SpecialKey      guifg=#2040a0     guibg=#f1f1f1
highlight Directory       guifg=#2040a0     guibg=#f1f1f1
highlight MatchParen      guifg=#2040a0     guibg=#f1f1f1 gui=bold

" Green keywords
highlight Todo            guifg=#20a040     guibg=#f1f1f1 gui=italic

" Interface colors
highlight LineNr          guifg=#b0b0b0     guibg=#f1f1f1
highlight CursorLineNr    guifg=#b0b0b0     guibg=#f1f1f1
highlight Pmenu           guifg=#f1f1f1     guibg=#424242
highlight Folded          guifg=#424242     guibg=#f1f1f1  gui=bold
highlight StatusLine      guifg=#424242     guibg=#f1f1f1  gui=reverse
highlight StatusLineNC    guifg=#aaaaaa     guibg=#f1f1f1  gui=underline
highlight VertSplit       guifg=#424242     guibg=#f1f1f1  gui=NONE
highlight WildMenu        guifg=#424242     guibg=#ffff88  gui=bold

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
