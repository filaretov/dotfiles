---@type vim.lsp.Config
return {
  cmd = { "nu", "--lsp" },
  filetypes = { "nu" },
  root_markers = { ".git", "*.nu" },
  single_file_support = true,
}
