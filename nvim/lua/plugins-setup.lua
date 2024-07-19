local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	{
		'goolord/alpha-nvim',
		dependencies = { 'nvim-tree/nvim-web-devicons' },
		config = function()
			require 'alpha'.setup(require 'alpha.themes.startify'.config)
		end
	},
	"nvim-lualine/lualine.nvim",
	{ "catppuccin/nvim",            name = "catppuccin", priority = 1000 },
	{ "dundalek/lazy-lsp.nvim" },
	{ "nvim-tree/nvim-web-devicons" },
	{ 'VonHeikemen/lsp-zero.nvim',  branch = 'v3.x' },
	{ 'neovim/nvim-lspconfig' },
	{ 'hrsh7th/cmp-nvim-lsp' },
	{ 'hrsh7th/nvim-cmp' },
	{
		'windwp/nvim-autopairs',
		event = "InsertEnter",
		config = true
		-- use opts = {} for passing setup options
		-- this is equalent to setup({}) function
	},
	{ 'L3MON4D3/LuaSnip' },

	{ 'nvim-treesitter/nvim-treesitter' },
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = { "nvim-lua/plenary.nvim", "BurntSushi/ripgrep", "sharkdp/fd" },
		keys = {
			{ "<C-t>", "<CMD>Telescope<CR>",             mode = { "n", "i", "v" } },
			{ "<C-f>", "<CMD>Telescope find_files<CR>",  mode = { "n", "i", "v" } },
			{ "<C-l>", "<CMD>Telescope live_grep<CR>",   mode = { "n", "i", "v" } },
			{ "<C-c>", "<CMD>Telescope commands<CR>",    mode = { "n", "i", "v" } },
			{ "<C-k>", "<CMD>Telescope keymaps<CR>",     mode = { "n", "i", "v" } },
			{ "<C-s>", "<CMD>Telescope grep_string<CR>", mode = { "n", "i", "v" } },
		},
		config = true
	},
	"smartpde/telescope-recent-files",
	{
		"sindrets/diffview.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{ "TimUntersberger/neogit", config = { disable_commit_confirmation = true } },
		},
		keys = {
			{ "<C-g>", "<CMD>DiffviewOpen<CR>", mode = { "n", "i", "v" } }
		},
		config = {
			keymaps = {
				view = {
					["<C-g>"] = "<CMD>DiffviewClose<CR>",
					["c"] = "<CMD>DiffviewClose|Neogit commit<CR>",
				},
				file_panel = {
					["<C-g>"] = "<CMD>DiffviewClose<CR>",
					["c"] = "<CMD>DiffviewClose|Neogit commit<CR>",
				},
			},
		}
	},
	{
		"andymass/vim-matchup",
		setup = function()
			vim.g.matchup_matchparen_offscreen = { method = "popup" }
		end,
	},
	{
		"nvim-telescope/telescope-file-browser.nvim",
		dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
	},
	{
		"epwalsh/obsidian.nvim",
		version = "*", -- recommended, use latest release instead of latest commit
		lazy = true,
		ft = "markdown",
		-- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
		-- event = {
		--   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
		--   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/**.md"
		--   "BufReadPre path/to/my-vault/**.md",
		--   "BufNewFile path/to/my-vault/**.md",
		-- },
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},
})
vim.cmd.colorscheme "catppuccin-mocha"
require("lualine").setup()
