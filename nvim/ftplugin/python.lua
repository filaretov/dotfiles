config = {
  name = "pyright",
  cmd = { 'pyright-langserver', '--stdio' },
  filetypes = { 'python' },
  root_dir = vim.fs.dirname(vim.fs.find({'pyproject.toml', 'setup.py'}, { upward = true })[1]),
  single_file_support = true,
  settings = {
    python = {
      analysis = {
        autoSearchPaths = true,
        useLibraryCodeForTypes = true,
        diagnosticMode = 'openFilesOnly',
      },
    },
  },
}

function PyrightOrganizeImports()
  local params = {
    command = 'pyright.organizeimports',
    arguments = { vim.uri_from_bufnr(0) },
  }
  vim.lsp.buf.execute_command(params)
end

function PyrightSetPythonPath(path)
  local clients = vim.lsp.get_active_clients {
    bufnr = vim.api.nvim_get_current_buf(),
    name = 'pyright',
  }
  for _, client in ipairs(clients) do
    client.config.settings = vim.tbl_deep_extend('force', client.config.settings, { python = { pythonPath = path } })
    client.notify('workspace/didChangeConfiguration', { settings = nil })
  end
end

local bufname = vim.fs.basename(vim.fn.bufname('%'))

local invoke_python = "/Users/hgf/.local/pipx/venvs/invoke/bin/python"

if bufname == 'tasks.py' then
  config.settings = vim.tbl_deep_extend('force', config.settings, { python = { pythonPath = invoke_python } })
  config.name = "pyright-invoke"
end

vim.cmd('command! -nargs=1 PyrightSetPythonPath lua PyrightSetPythonPath(<args>)')
vim.cmd('command! PyrightOrganizeImports lua PyrightOrganizeImports()')

vim.lsp.start(config)
