" vim: foldmethod=marker foldlevelstart=0:

let g:colors_name = "vetinari"
set background=dark

let  s:blue    =  "#2D9EC8"
let  s:cyan    =  "#81DAE7"
let  s:green   =  "#54DE7B"
let  s:lime    =  "#80D3A4"
let  s:red     =  "#D93B4A"
let  s:white   =  "#F0F0F0"
let  s:black   =  "#2E3440"
let  s:yellow  =  "#EAEFD0"
let  s:grey    =  "#888888"

let s:bold = "bold,"
let s:italic = "italic,"
let s:none = ""

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

" Base Groups {{{

call  s:hi(  "Normal",       s:black,  s:white,  s:italic )
call  s:hi(  "EndOfBuffer",  s:black,  s:white,  ""       )
call  s:hi(  "LineNr",       "",       s:grey,   ""       )

call  s:hi(  "Bold",         "",       "",       s:bold   )

" }}}

" Non Text Elements {{{


call  s:hi(  "ColorColumn",   s:black,  "",  ""  )
call  s:hi(  "CursorLine",    s:black,  "",  ""  )
call  s:hi(  "NonText",       s:black,  "",  ""  )
call  s:hi(  "ColorColumn",   s:black,  "",  ""  )
call  s:hi(  "Cursor",        s:black,  "",  ""  )
call  s:hi(  "CursorLine",    s:black,  "",  ""  )
call  s:hi(  "Error",         s:black,  "",  ""  )
call  s:hi(  "iCursor",       s:black,  "",  ""  )
call  s:hi(  "LineNr",        s:black,  "",  ""  )
call  s:hi(  "MatchParen",    s:black,  "",  ""  )
call  s:hi(  "NonText",       s:black,  "",  ""  )
call  s:hi(  "Normal",        s:black,  "",  ""  )
call  s:hi(  "PMenu",         s:black,  "",  ""  )
call  s:hi(  "PmenuSbar",     s:black,  "",  ""  )
call  s:hi(  "PMenuSel",      s:white,  s:black,  ""  )
call  s:hi(  "PmenuThumb",    s:black,  "",  ""  )
call  s:hi(  "SpecialKey",    s:black,  "",  ""  )
call  s:hi(  "SpellBad",      s:black,  "",  ""  )
call  s:hi(  "SpellCap",      s:black,  "",  ""  )
call  s:hi(  "SpellLocal",    s:black,  "",  ""  )
call  s:hi(  "SpellRare",     s:black,  "",  ""  )
call  s:hi(  "Visual",        s:black,  "",  ""  )
call  s:hi(  "VisualNOS",     s:black,  "",  ""  )
call  s:hi(  "CursorColumn",  s:black,  "",  ""  )
call  s:hi(  "CursorLineNr",  s:black,  "",  ""  )
call  s:hi(  "CursorLineNr",  s:black,  "",  ""  )
call  s:hi(  "Folded",        s:black,  "",  ""  )
call  s:hi(  "FoldColumn",    s:black,  "",  ""  )
call  s:hi(  "SignColumn",    s:black,  "",  ""  )
call  s:hi(  "VertSplit",     s:black,  s:black,  ""  )
call  s:hi(  "StatusLine",    s:white,  s:black,  ""  )
call  s:hi(  "StatusLineNC",  s:grey,  s:black,  ""  )

highlight! link Folded Bold
highlight! clear Conceal

" }}}

" Search {{{

call  s:hi("IncSearch",  s:white,  "",  ""  )
call  s:hi("Search",     s:white,  "",  ""  )

" }}}

" Language Base Groups {{{

call  s:hi(  "Boolean",         "",  s:white,  ""  )
call  s:hi(  "Character",       "",  s:white,  ""  )
call  s:hi(  "Comment",         "",  s:lime,   ""  )
call  s:hi(  "Conditional",     "",  s:white,  ""  )
call  s:hi(  "Constant",        "",  s:white,  ""  )
call  s:hi(  "Define",          "",  s:white,  ""  )
call  s:hi(  "Delimiter",       "",  s:white,  ""  )
call  s:hi(  "Exception",       "",  s:white,  ""  )
call  s:hi(  "Float",           "",  s:white,  ""  )
call  s:hi(  "Function",        "",  s:white,  ""  )
call  s:hi(  "Identifier",      "",  s:white,  ""  )
call  s:hi(  "Include",         "",  s:white,  ""  )
call  s:hi(  "Keyword",         "",  s:white,  ""  )
call  s:hi(  "Label",           "",  s:white,  ""  )
call  s:hi(  "Number",          "",  s:white,  ""  )
call  s:hi(  "Operator",        "",  s:white,  ""  )
call  s:hi(  "PreProc",         "",  s:white,  ""  )
call  s:hi(  "Repeat",          "",  s:white,  ""  )
call  s:hi(  "Special",         "",  s:white,  ""  )
call  s:hi(  "SpecialChar",     "",  s:white,  ""  )
call  s:hi(  "SpecialComment",  "",  s:white,  ""  )
call  s:hi(  "Statement",       "",  s:white,  ""  )
call  s:hi(  "StorageClass",    "",  s:white,  ""  )
call  s:hi(  "String",          "",  s:white,   ""  )
call  s:hi(  "Structure",       "",  s:white,  ""  )
call  s:hi(  "Tag",             "",  s:white,  ""  )
call  s:hi(  "Todo",            "",  s:white,  ""  )
call  s:hi(  "Type",            "",  s:white,  ""  )
call  s:hi(  "Typedef",         "",  s:white,  ""  )

" }}}

