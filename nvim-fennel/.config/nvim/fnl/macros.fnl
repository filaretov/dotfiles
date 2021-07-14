(local fun (require :fnl.functions))

(fn set-opt [scope name value]
  (assert (sym? name))
  (let [opt (view name)]
    `(tset ,scope ,opt ,(if (fun.nil? value) true value))))

(fn opt [name value]
  "Set Vim option (essentially global)"
  (set-opt `vim.o name value))

(fn wopt [name value]
  "Set Vim window option (such as number)"
  (set-opt `vim.wo name value))

(fn bopt [name value]
  "Set Vim buffer option (such as shiftwidth)"
  (set-opt `vim.bo name value))

(fn into-func [code]
  "Place Fennel code into Neovim Lua function

  Example:
  >>> (into-func (vim.inspect a_table))
  return \"lua vim.inspect(a_table)\"
  "
  `(.. "lua " ((. (require :functions) :mkfn) (fn [] ,code)) "()"))

(fn cmd [code]
  "
  Place Fennel code into Neovim Lua suitable for a keymap.

  Example:
  >>> (key n "gm" (cmd (some-function)))
  "
  `(.. "<cmd>" ,(into-func code) "<cr>"))

(fn call [module function]
  `((. (require ,module) ,(view function))))

(fn augroup [name ...]
  "Define a Vim auto group."
  `(do
     (vim.api.nvim_command ,(.. "augroup " (view name)))
     (vim.api.nvim_command "au!")
     ,[...]
     (vim.api.nvim_command "augroup END")))


(fn key [mode lhr rhs opts]
  "Create a Vim mapping.

  All mappings are noremap by default.

  Example:
  >>> (map n \"<C-s\" \":<C-u>w<cr>\")
  "
  (let [modes (if (sequence? mode) (icollect [_ v (ipairs mode)] (view v)) [(view mode)])]
    `(each [_# m# (ipairs ,modes)]
      (vim.api.nvim_set_keymap m# ,lhr ,rhs ,(or opts {:noremap true})))))


(fn on [events pattern ...]
  "Run ... on events if file matches pattern (defines a Vim autocommand)

  Args:
      events: list of symbols that map to Vim events.
      pattern: file pattern
      ...: code to run on event
  Example:
  >>> (autocmd [BufRead BufNewFile \"*.md\" \"set filetype=markdown\")
  "
  (assert (sequence? events))
  (let [events (table.concat (fun.map view events) ",")]
    `(vim.api.nvim_exec (table.concat ["autocmd" ,events ,pattern ,(into-func ...)] " ") false)))


{: set-opt
 : opt
 : wopt
 : bopt
 : key
 : on
 : augroup
 : cmd
 : call
 }
