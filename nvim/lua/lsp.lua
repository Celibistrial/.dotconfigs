---------------------------------------------------------------------
-- 🌟 Lazy-LSP Setup
-- Handles automatic LSP server setup using mason/other backends
---------------------------------------------------------------------
require("lazy-lsp").setup {
	excluded_servers = {
		"ccls",
		"denols",
		"docker_compose_language_service",
		"flow",
		"ltex",
		"quick_lint_js",
		"rnix",
		"scry",
		-- "tailwindcss",
		"nil_ls",
	},
	prefer_local = true,
	default_config = {
		flags = { debounce_text_changes = 150 },
	},
	configs = {
		lua_ls = {
			settings = {
				Lua = { diagnostics = { globals = { "vim" } } },
			},
		},
		nixd = {
			settings = {
				nixd = { formatting = { command = { "alejandra" } } },
			},
		},
	},
}

---------------------------------------------------------------------
-- 🌟 nvim-cmp Setup (Completion + Snippets)
---------------------------------------------------------------------
require("luasnip.loaders.from_vscode").lazy_load({ paths = "./snippets" })
local luasnip = require("luasnip")

---------------------------------------------------------------------
-- 🌟 Conform.nvim Setup
---------------------------------------------------------------------
require("conform").setup({
	formatters_by_ft = {
		lua        = { "stylua" },
		python     = { "isort", "black" },
		rust       = { "rustfmt", lsp_format = "fallback" },
		javascript = { "prettierd", "prettier", stop_after_first = true },
		nix        = { "alejandra" },
	},
})

vim.keymap.set("n", "cf", function()
	require("conform").format({ lsp_fallback = true, async = true, timeout_ms = 1000 })
end, { desc = "Format with Conform" })

---------------------------------------------------------------------
-- 🌟 LSP UI Tweaks
---------------------------------------------------------------------
vim.lsp.handlers["textDocument/hover"] =
    vim.lsp.with(vim.lsp.handlers.hover, { border = "rounded" })
vim.lsp.handlers["textDocument/signatureHelp"] =
    vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })

vim.opt.signcolumn = "yes"
vim.diagnostic.config({
	virtual_text = false,
	signs = true,
	underline = true,
	update_in_insert = false,
	severity_sort = true,
})
