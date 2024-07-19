return require("packer").startup(function(use)
	use("nvim-treesitter/nvim-treesitter")
	-- use({
	-- 	"lukas-reineke/headlines.nvim",
	-- 	after = "nvim-treesitter",
	-- 	config = function()
	-- 		require("headlines").setup()
	-- 	end,
	-- })
	use("nvim-lua/plenary.nvim")
	-- Color Schemes
	use({
		"catppuccin/nvim",
		as = "catppuccin",
		config = function()
      vim.g.catppuccin_flavour = "macchiato" -- latte, frappe, macchiato, mocha
			require("catppuccin").setup()

			vim.cmd([[colorscheme catppuccin]])
		end,
	})
	-- fix for kitty borders
	use({
		"shaun-mathew/Chameleon.nvim",
		config = function()
			if vim.g.neovide == nil then
				require("chameleon").setup()
			end
		end,
	})
	-- file explorer
	use({
		"nvim-tree/nvim-tree.lua",
		requires = {
			"nvim-tree/nvim-web-devicons", -- optional
		},
		config = function()
			require("nvim-tree").setup({})
		end,
	})
	-- comment lines
	use({
		"terrortylor/nvim-comment",
		config = function()
			require("nvim_comment").setup()
		end,
	})
	-- status bar (for funzies)
	use({
		"nvim-lualine/lualine.nvim",
	})
	-- startify replacement
	use({
		"goolord/alpha-nvim",
		requires = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("alpha").setup(require("alpha.themes.startify").config)
		end,
	})
	-- lsp
	use({
		"williamboman/mason.nvim",
	})
	use("neovim/nvim-lspconfig")
	use({
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({})
		end,
	})
	use("RRethy/vim-illuminate")
	use({
		"williamboman/mason-lspconfig.nvim",
	})

	use({
		"L3MON4D3/LuaSnip",
		-- follow latest release.
		-- install jsregexp (optional!:).
		run = "make install_jsregexp",
	})
	use("hrsh7th/nvim-cmp")
	use({
		"hrsh7th/cmp-buffer",
	})
	use({
		"hrsh7th/cmp-path",
	})
	use("saadparwaiz1/cmp_luasnip")
	use("hrsh7th/cmp-nvim-lsp")
	use("nvim-telescope/telescope.nvim")
	use({
		"machakann/vim-sandwich",
	})
	use({ "hrsh7th/cmp-nvim-lua" })
	use("jose-elias-alvarez/null-ls.nvim")
	use({
		"folke/trouble.nvim",
	})
end)
