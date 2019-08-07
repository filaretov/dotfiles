" SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
" SPDX-License-Identifier: MIT
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

" GUI
highlight Normal          guifg=#f1f1f1     guibg=#424242  gui=NONE

highlight Search          guifg=#f1f1f1     guibg=#8a8a8a
highlight IncSearch       guifg=#f1f1f1     guibg=#8a8a8a  gui=NONE

highlight Visual          guibg=#6a6a6a

highlight Cursor          guifg=#0000ff     guibg=#424242

highlight Comment         guifg=#aaaaaa     guibg=#424242 gui=italic
highlight NonText         guifg=#aaaaaa     guibg=#424242 gui=NONE
highlight Whitespace      guifg=#aaaaaa     guibg=#424242 gui=NONE

" Bold
highlight Title           guifg=#f1f1f1     guibg=#424242 gui=bold
highlight Statement       guifg=#f1f1f1     guibg=#424242 gui=bold
highlight Identifier      guifg=#f1f1f1     guibg=#424242 gui=bold

" Blue
highlight PreProc         guifg=#80b0f0     guibg=#424242
highlight Constant        guifg=#80b0f0     guibg=#424242
highlight SpecialKey      guifg=#80b0f0     guibg=#424242
highlight Directory       guifg=#80b0f0     guibg=#424242
highlight MatchParen      guifg=#80b0f0     guibg=#424242 gui=bold
highlight Underlined      guifg=#80b0f0     guibg=#424242 gui=underline,NONE

" Green
highlight Todo            guifg=#60f0a0     guibg=#424242 gui=italic
highlight Type            guifg=#60f0a0     guibg=#424242 gui=NONE

" Red
highlight ErrorMsg        guifg=#e04040     guibg=#424242 gui=reverse

" Yellow
highlight Special         guifg=#c0a040     guibg=#424242

" Magenta

" Interface
highlight Cursor          guifg=#aaaaaa     guibg=#424242  gui=NONE
highlight LineNr          guifg=#bababa     guibg=#424242
highlight CursorLineNr    guifg=#bababa     guibg=#424242
highlight Pmenu           guifg=#424242     guibg=#f1f1f1
highlight Folded          guifg=#f1f1f1     guibg=#424242  gui=bold
highlight StatusLine      guifg=#f1f1f1     guibg=#424242  gui=bold,underline
highlight StatusLineNC    guifg=#bababa     guibg=#424242  gui=underline
highlight VertSplit       guifg=#f1f1f1     guibg=#424242  gui=NONE
highlight WildMenu        guifg=#f1f1f1     guibg=#ffff88  gui=bold
highlight SignColumn      guifg=#f1f1f1     guibg=#424242  gui=bold

" HTML
highlight htmlBold        guifg=#f1f1f1     guibg=#424242 gui=bold


" Neovim
let g:terminal_color_0 = "#424242"
let g:terminal_color_1 = "#e04040"
let g:terminal_color_2 = "#20a060"
let g:terminal_color_3 = "#f0d060"
let g:terminal_color_4 = "#2060a0"
let g:terminal_color_5 = "#a060a0"
let g:terminal_color_6 = "#20c0c0"
let g:terminal_color_7 = "#f1f1f1"
