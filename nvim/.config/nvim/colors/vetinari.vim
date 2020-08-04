" vim: foldmethod=marker foldlevelstart=0:

" Setup {{{

let g:colors_name = "vetinari"
set background=dark

let s:black  = "#000000"
let s:white  = "#FFFFFF"
let s:grey   = "#808080"
let s:red    = "#BB5040"

let s:bold = "bold,"
let s:italic = "italic,"
let s:none = 'NONE'

hi clear
syntax reset

function! s:hi(group, guibg, guifg, attr)
  if a:guibg != ""
    exec "hi " . a:group . " guibg=" . a:guibg
  endif
  if a:guifg != ""
    exec "hi " . a:group . " guifg=" . a:guifg
  endif
  if a:attr != ""
    exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
  endif
endfunction

" }}}

" Vetinari Groups {{{
call s:hi('VetinariNormal', s:black, s:white, s:none)
call s:hi('VetinariBlack', s:black, s:black, s:none)
call s:hi('VetinariGrey', s:black, s:grey, s:none)
call s:hi('VetinariRed', s:black, s:red, s:none)
" }}}

" Base Groups {{{

hi! link Normal VetinariNormal

hi! link EndOfBuffer VetinariNormal
hi! link LineNr VetinariNormal
hi! link Bold VetinariNormal

" }}}

" Non Text Elements {{{

hi! link EndOfBuffer VetinariBlack
hi! link ColorColumn VetinariNormal
hi! link CursorLine VetinariNormal
hi! link NonText VetinariNormal
hi! link ColorColumn VetinariNormal
hi! link Cursor VetinariNormal
hi! link CursorLine VetinariNormal
hi! link Error VetinariNormal
hi! link iCursor VetinariNormal
hi! link LineNr VetinariNormal
hi! link MatchParen VetinariRed
hi! link NonText VetinariNormal
hi! link Normal VetinariNormal
hi! link PMenu VetinariNormal
hi! link PmenuSbar VetinariNormal
hi! link PMenuSel VetinariNormal
hi! link PmenuThumb VetinariNormal
hi! link SpecialKey VetinariNormal
hi! link SpellBad VetinariNormal
hi! link SpellCap VetinariNormal
hi! link SpellLocal VetinariNormal
hi! link SpellRare VetinariNormal
hi! link Visual VetinariNormal
hi! link VisualNOS VetinariNormal
hi! link CursorColumn VetinariNormal
hi! link CursorLineNr VetinariNormal
hi! link CursorLineNr VetinariNormal
hi! link Folded VetinariNormal
hi! link FoldColumn VetinariNormal
hi! link SignColumn VetinariNormal
hi! link VertSplit VetinariNormal
hi! link StatusLine VetinariNormal
hi! link StatusLineNC VetinariNormal

hi! link Folded Comment
highlight! clear Conceal

" }}}

" Search {{{
" }}}

" PL Base Groups {{{
hi!  link  Boolean         VetinariNormal
hi!  link  Character       VetinariNormal
hi!  link  Comment         VetinariGrey
hi!  link  Conditional     VetinariNormal
hi!  link  Constant        VetinariNormal
hi!  link  Define          VetinariNormal
hi!  link  Delimiter       VetinariNormal
hi!  link  Exception       VetinariNormal
hi!  link  Function        VetinariNormal
hi!  link  Identifier      VetinariNormal
hi!  link  Include         VetinariNormal
hi!  link  Keyword         VetinariNormal
hi!  link  Label           VetinariNormal
hi!  link  Float           VetinariNormal
hi!  link  Number          VetinariNormal
hi!  link  Operator        VetinariNormal
hi!  link  PreProc         VetinariNormal
hi!  link  Repeat          VetinariNormal
hi!  link  Special         VetinariNormal
hi!  link  SpecialChar     VetinariNormal
hi!  link  SpecialComment  VetinariNormal
hi!  link  Statement       VetinariRed
hi!  link  StorageClass    VetinariNormal
hi!  link  String          VetinariNormal
hi!  link  Structure       VetinariNormal
hi!  link  Tag             VetinariNormal
hi!  link  Todo            VetinariNormal
hi!  link  Type            VetinariNormal
hi!  link  Typedef         VetinariNormal

" }}}

" Text Base Groups {{{
hi! link Title VetinariNormal
hi! link Underlined VetinariNormal
hi! link ErrorMsg VetinariNormal
hi! link WarningMsg VetinariNormal
" }}}

" Neovim terminal {{{

" }}}

" Pandoc {{{
hi! link pandocHTMLCommentStart VetinariNormal
hi! link pandocHTMLCommentEnd VetinariNormal
hi! link pandocStrong VetinariNormal
hi! link pandocEmphasis VetinariNormal
" }}}

" Org {{{
hi! link org_todo_keyword_TODO VetinariNormal
hi! link org_todo_keyword_DONE VetinariNormal
" }}}

" Ledger {{{
hi! link ledgerTransactionDate VetinariNormal
" }}}

" Elixir {{{
hi! link elixirAtom VetinariNormal
hi! link elixirBlockDefinition VetinariNormal
hi! link elixirDefine VetinariNormal
hi! link elixirModuleDefine VetinariNormal
hi! link elixirStringDelimiter String
" }}}
