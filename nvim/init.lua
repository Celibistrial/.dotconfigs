vim.loader.enable()

require('celi.vimscript')
require('celi.settings')
require('celi.keybinds')
require('celi.plugins')
require('celi.lualine')
require('celi.autopairs')
require('celi.telescope')
require('celi.cmp')
require('celi.lsp')
if vim.g.neovide then
  require('celi.neovide')
end

-- god knows what this does but it seems to fix somethign
vim.api.nvim_create_autocmd({ "VimLeave" }, {
  callback = function()
    vim.cmd('sleep 10m')
  end,
})
