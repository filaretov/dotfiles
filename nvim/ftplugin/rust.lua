config = {
  name = "rust-analyzer",
  cmd = { "rust-analyzer" },
  filetypes = { 'rust' },
  root_dir = vim.fs.dirname(vim.fs.find({'Cargo.toml'}, { upward = true })[1]),
}

vim.lsp.start(config)
