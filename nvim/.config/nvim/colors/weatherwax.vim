" vim: foldmethod=marker foldlevelstart=0:

" Setup {{{

let g:colors_name = "weatherwax"
set background=dark

let  s:blue    =  "#72A1BC"
let  s:cyan    =  "#81DAE7"
let  s:green   =  "#54DE7B"
let  s:lime    =  "#80D3A4"
let  s:red     =  "#DE6954"
let  s:white   =  "#F0F0F0"
let  s:black   =  "#2E3440"
let  s:yellow  =  "#D3CB80"
let  s:grey    =  "#888888"
let  s:orange  =  "#DE8A54"

let s:bold = "bold,"
let s:italic = "italic,"
let s:none = "NONE"

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

" Base Groups {{{

call  s:hi(  "Normal",       s:black,  s:white,  "" )
call  s:hi(  "EndOfBuffer",  s:black,  s:white,  ""       )
call  s:hi(  "LineNr",       "",       s:grey,   ""       )
call  s:hi(  "Bold",         "",       "",       s:bold   )

" }}}

" Non Text Elements {{{

call  s:hi(  "EndOfBuffer",   s:black,  s:grey, "" )
call  s:hi(  "ColorColumn",   s:black,  "",  ""  )
call  s:hi(  "CursorLine",    s:black,  "",  ""  )
call  s:hi(  "NonText",       s:black,  s:grey,  ""  )
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
call  s:hi(  "Visual",        s:grey,  "",  ""  )
call  s:hi(  "VisualNOS",     s:grey,  "",  ""  )
call  s:hi(  "CursorColumn",  s:black,  "",  ""  )
call  s:hi(  "CursorLineNr",  s:black,  "",  ""  )
call  s:hi(  "CursorLineNr",  s:black,  "",  ""  )
call  s:hi(  "Folded",        s:black,  "",  ""  )
call  s:hi(  "FoldColumn",    s:black,  "",  ""  )
call  s:hi(  "SignColumn",    s:black,  "",  ""  )
call  s:hi(  "VertSplit",     s:black,  s:black,  ""  )
call  s:hi(  "StatusLine",    s:black,  s:lime,  ""  )
call  s:hi(  "StatusLineNC",  s:black,  s:grey,  ""  )

call  s:hi(  "Folded",        "",       s:blue, "" )
highlight! clear Conceal

" }}}

" Search {{{
call  s:hi("IncSearch",  s:white,  "",  ""  )
call  s:hi("Search",     s:white,  "",  ""  )
" }}}

" PL Base Groups {{{

call  s:hi(  "Boolean",         "",  s:lime,  ""  )
call  s:hi(  "Character",       "",  s:white,  ""  )
call  s:hi(  "Comment",         "",  s:grey,   ""  )
call  s:hi(  "Conditional",     "",  s:orange,  ""  )
call  s:hi(  "Constant",        "",  s:red,    ""  )
call  s:hi(  "Define",          "",  s:white,  ""  )
call  s:hi(  "Delimiter",       "",  s:orange,  ""  )
call  s:hi(  "Exception",       "",  s:red,  ""  )
call  s:hi(  "Function",        "",  s:yellow,    ""  )
call  s:hi(  "Identifier",      "",  s:yellow,  ""  )
call  s:hi(  "Include",         "",  s:orange,  ""  )
call  s:hi(  "Keyword",         "",  s:orange,  ""  )
call  s:hi(  "Label",           "",  s:yellow,  ""  )
call  s:hi(  "Float",           "",  s:lime,  ""  )
call  s:hi(  "Number",          "",  s:lime,  ""  )
call  s:hi(  "Operator",        "",  s:orange,  ""  )
call  s:hi(  "PreProc",         "",  s:yellow,  ""  )
call  s:hi(  "Repeat",          "",  s:orange,  ""  )
call  s:hi(  "Special",         "",  s:white,  ""  )
call  s:hi(  "SpecialChar",     "",  s:white,  ""  )
call  s:hi(  "SpecialComment",  "",  s:white,  ""  )
call  s:hi(  "Statement",       "",  s:orange,  s:none )
call  s:hi(  "StorageClass",    "",  s:yellow,  ""  )
call  s:hi(  "String",          "",  s:lime,   ""  )
call  s:hi(  "Structure",       "",  s:white,  ""  )
call  s:hi(  "Tag",             "",  s:white,  ""  )
call  s:hi(  "Todo",            s:black,  s:yellow,  ""  )
call  s:hi(  "Type",            "",  s:orange, s:none  )
call  s:hi(  "Typedef",         "",  s:white,  ""  )

" }}}

" Text Base Groups {{{
call s:hi("Title", "", s:orange, "")
call s:hi("Underlined", "", s:blue, "")
call s:hi("ErrorMsg", s:red, s:white, "")
call s:hi("WarningMsg", "", s:red, "")
" }}}

" Neovim terminal {{{

" }}}

" Pandoc {{{
call s:hi( "pandocHTMLCommentStart", "", s:grey, "")
call s:hi( "pandocHTMLCommentEnd", "", s:grey, "")
call s:hi( "pandocStrong", "", "", s:bold)
call s:hi( "pandocEmphasis", "", "", s:italic)
" }}}

" Org {{{
call s:hi( "org_todo_keyword_TODO", "", s:yellow, "")
call s:hi( "org_todo_keyword_DONE", "", s:lime, "")
" }}}

" Ledger {{{
call s:hi( "ledgerTransactionDate", "", s:orange, "")
" }}}

" Elixir {{{
call s:hi( "elixirAtom", "", s:lime, "")
call s:hi( "elixirBlockDefinition", "", s:orange, "")
call s:hi( "elixirDefine", "", s:orange, "")
call s:hi( "elixirModuleDefine", "", s:orange, "")
hi! link elixirStringDelimiter String
" }}}
