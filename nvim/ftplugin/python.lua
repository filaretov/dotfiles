local config = vim.deepcopy(vim.lsp.config.basedpyright)

local name = "basedpyright"
if vim.fs.basename(vim.api.nvim_buf_get_name(0)) == "tasks.py" then
  local invoke = '/Users/hgf/Library/Application Support/uv/tools/invoke/bin/python'
  config.settings = vim.tbl_deep_extend(
    'force',
    config.settings, { python = { pythonPath = invoke } }
  )
  name = "basedpyright-invoke"
end

config.name = name

vim.lsp.start(config)
