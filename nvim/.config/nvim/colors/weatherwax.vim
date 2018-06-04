" Vim color file
" Maintainer: Hristo Filaretov <h.filaretov@protonmail.com>
" Colors
" #424242 black
" #802020 red
" #108040 green
" #e08040 yellow
" #2060a0 blue
" #704070 magenta
" #2090b0 cyan
" #bababa light grey
" #808080 grey
" #e04040 light red
" #20a060 light green
" #c0a040 light yellow
" #80aaee light blue
" #a060a0 ligt magenta
" #20c0c0 light cyan
" #fafafa white

set background=light
highlight clear
if exists("syntax_on")
  syntax reset
endif

"colorscheme default
let g:colors_name = "weatherwax"

" GUI
highlight Normal          guifg=#424242     guibg=#fafafa  gui=none

highlight Search          guifg=#424242     guibg=#ffff88
highlight IncSearch       guifg=#424242     guibg=#ffff88  gui=none

highlight Visual          guibg=#bababa

highlight Cursor          guifg=#0000ff     guibg=#424242

highlight Comment         guifg=#aaaaaa     guibg=#fafafa gui=italic
highlight NonText         guifg=#aaaaaa     guibg=#fafafa gui=none
highlight Whitespace      guifg=#aaaaaa     guibg=#fafafa gui=none

" Bold
highlight Title           guifg=#424242     guibg=#fafafa gui=bold

" Blue
highlight PreProc         guifg=#2060a0     guibg=#fafafa
highlight Constant        guifg=#2060a0     guibg=#fafafa
highlight SpecialKey      guifg=#2060a0     guibg=#fafafa
highlight Directory       guifg=#2060a0     guibg=#fafafa
highlight MatchParen      guifg=#2060a0     guibg=#fafafa gui=bold
highlight Underlined      guifg=#2060a0     guibg=#fafafa gui=underline,none

" Green
highlight Todo            guifg=#108040     guibg=#fafafa gui=italic
highlight Type            guifg=#108040     guibg=#fafafa gui=none

" Red
highlight Error           guifg=#e04040     guibg=#fafafa gui=none
highlight ErrorMsg        guifg=#e04040     guibg=#fafafa gui=none

" Light Yellow
highlight Special         guifg=#c0a040     guibg=#fafafa

" Yellow
highlight Identifier      guifg=#e08040     guibg=#fafafa gui=none

" Light Magenta
highlight Statement       guifg=#a060a0     guibg=#fafafa gui=bold

" Interface
highlight Cursor          guifg=#aaaaaa     guibg=#fafafa  gui=none
highlight LineNr          guifg=#bababa     guibg=#fafafa
highlight CursorLineNr    guifg=#bababa     guibg=#fafafa
highlight Pmenu           guifg=#fafafa     guibg=#424242
highlight Folded          guifg=#424242     guibg=#fafafa  gui=bold
highlight StatusLine      guifg=#fafafa     guibg=#808080  gui=none
highlight StatusLineNC    guifg=#808080     guibg=#e0e0e0  gui=none
highlight VertSplit       guifg=#424242     guibg=#fafafa  gui=none
highlight WildMenu        guifg=#424242     guibg=#ffff88  gui=bold
highlight SignColumn      guifg=#424242     guibg=#fafafa  gui=bold

" HTML
highlight htmlBold        guifg=#424242     guibg=#fafafa gui=bold


" Neovim
let g:terminal_color_0 = "#424242"
let g:terminal_color_1 = "#e04040"
let g:terminal_color_2 = "#20a060"
let g:terminal_color_3 = "#f0d060"
let g:terminal_color_4 = "#2060a0"
let g:terminal_color_5 = "#a060a0"
let g:terminal_color_6 = "#20c0c0"
let g:terminal_color_7 = "#fafafa"
