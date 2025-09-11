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
	"nvim-lualine/lualine.nvim",
	{
		"stevearc/conform.nvim",
		opts = {},
	},
	-- lazy.nvim
	{
		"dundalek/lazy-lsp.nvim",
		dependencies = { "neovim/nvim-lspconfig" },
		config = function()
			require("lazy-lsp").setup({})
		end,
	},
	{
		"dundalek/lazy-lsp.nvim",
		dependencies = { "neovim/nvim-lspconfig" },
		config = function()
			require("lazy-lsp").setup({})
		end,
	},
	"nvim-tree/nvim-web-devicons",
	{
		"goolord/alpha-nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("alpha").setup(require("alpha.themes.startify").config)
		end,
	},
	{ "catppuccin/nvim",             name = "catppuccin", priority = 1000 },
	{ "dundalek/lazy-lsp.nvim" },
	{
		"fredehoey/tardis.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = true,
	},
	{ "neovim/nvim-lspconfig" },
	{ "hrsh7th/cmp-nvim-lsp" },
	{ "hrsh7th/cmp-buffer" },
	{ "hrsh7th/cmp-path" },
	{ "saadparwaiz1/cmp_luasnip" },
	{ "hrsh7th/nvim-cmp" },
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = true,
		-- use opts = {} for passing setup options
		-- this is equalent to setup({}) function
	},
	{ "L3MON4D3/LuaSnip" },
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")

			configs.setup({
				ensure_installed = { "c", "lua", "vim", "vimdoc", "javascript", "html" },
				sync_install = false,
				highlight = { enable = true },
				indent = { enable = false },
			})
		end,
	},
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = { "nvim-lua/plenary.nvim", "BurntSushi/ripgrep", "sharkdp/fd" },
		config = true,
	},
	"smartpde/telescope-recent-files",
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim", -- required
			"sindrets/diffview.nvim", -- optional - Diff integration
			"nvim-telescope/telescope.nvim", -- optional
		},
	},
	{
		"andymass/vim-matchup",
		opts = {
			matchup_matchparen_offscreen = { method = "popup" },
		},
	},
	{
		"nvim-telescope/telescope-file-browser.nvim",
		dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
	},
})
vim.cmd.colorscheme("catppuccin-mocha")
require("lualine").setup()
