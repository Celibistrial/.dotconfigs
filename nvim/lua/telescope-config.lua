require("telescope").load_extension "file_browser"
require("telescope").load_extension("recent_files")

-- vim.keymap.set("n", "<space>fb", ":Telescope file_browser path=%:p:h select_buffer=true<CR>")

-- Map a shortcut to open the picker.
-- vim.api.nvim_set_keymap("n", "<Leader><Leader>",
--   [[<cmd>lua require('telescope').extensions.recent_files.pick()<CR>]],
--   {noremap = true, silent = true})
--
local builtin = require('telescope.builtin')
-- vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
vim.keymap.set('n', '<leader><leader>', builtin.find_files, { desc = 'Telescope find files' })
vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
vim.keymap.set('n', '<leader>fB', builtin.buffers, { desc = 'Telescope buffers' })
-- vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope current_buffer_fuzzy_find' })
vim.keymap.set('n', '<leader>fb', require('telescope.builtin').current_buffer_fuzzy_find,
	{ noremap = true, silent = true, desc = "Fuzzy find in current buffer" })
vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })

-- Escape telescope on the first ESC
local actions = require("telescope.actions")

require("telescope").setup({
	defaults = {
		mappings = {
			i = {
				["<esc>"] = actions.close,
			},
		},
	},
})
