(global __nvim_functions__ {})

(local vim _G.vim)

(fn mkfn [func]
  "Register a Fennel function as a callable Lua function from nvim. "
  (let [idx (+ (length __nvim_functions__) 1)]
  (tset __nvim_functions__ idx func)
  (.. "__nvim_functions__[" idx "]")))


(fn map [f tbl]
  "Map function f to each element of tbl.

  This assumes a sequential table. Perhaps a keyword table should also be supported?

  Example:
  >>> (map #(+ $1 1) [1 2 3 4 5 6])
  "
  (let [new-tbl []]
    (each [_ v (ipairs tbl)]
          (table.insert new-tbl (f v)))
    new-tbl))

(fn nil? [v]
  (= nil v))


(fn dump [...]
  "Prints all passed objects :poop:"
  (each [_ v (ipairs [...])]
        (print (vim.inspect v))))

(fn make-fennel []
  "Compile all Fennel files and load the startup Lua file."
  (let [config (vim.fn.stdpath "config")]
    (vim.cmd (.. "!make --makefile "
                 config
                 "/makefile "
                 "--directory "
                 config))
    (vim.cmd (table.concat ["luafile" (.. config "/lua/startup.lua")] " "))))

(fn telescope-find-here []
  "Find files in the current file's directory"
  (let [fun (. (require :telescope.builtin) :find_files)]
    (fun {:search_dirs [(_G.vim.fn.expand "%:p:h")]})))

(fn edit [filename]
  (vim.cmd (.. "edit " filename)))

(fn normal [cmd]
  (vim.cmd (.. "normal " cmd)))

(fn config [filename]
  (.. (vim.fn.stdpath "config") "/" filename))

(fn end-of-line [] (normal "$"))
(fn beginning-of-line [] (normal "^"))
(fn save-buffer [] (vim.cmd "w"))

{
 : dump
 : make-fennel
 : telescope-find-here
 : mkfn
 : nil?
 : map
 : edit
 : config
 : end-of-line
 : beginning-of-line
 : save-buffer
 }
