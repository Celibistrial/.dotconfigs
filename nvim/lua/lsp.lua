---------------------------------------------------------------------lsp
-- 🌟 Lazy-LSP Setup
-- Handles automatic LSP server setup using mason/other backends
---------------------------------------------------------------------
require("lazy-lsp").setup {
	excluded_servers = {
		"ccls",
		"sourcekit",
		"denols",
		"docker_compose_language_service",
		"flow",
		"ltex",
		"quick_lint_js",
		"rnix",
		"scry",
		"tailwindcss",
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
-- 🌟 Snippets
---------------------------------------------------------------------
require("luasnip.loaders.from_vscode").lazy_load({ paths = "./snippets" })
local luasnip = require("luasnip")

---------------------------------------------------------------------
-- 🌟 LSP Keymaps
---------------------------------------------------------------------
vim.api.nvim_set_keymap("n", "<CR>", "<CR><Cmd>cclose<CR>", { noremap = false, silent = true })
vim.api.nvim_set_keymap("n", "<ESC>", "<Cmd>cclose<CR>", { noremap = false, silent = true })
vim.api.nvim_create_autocmd("LspAttach", {

	callback = function()
		local bufmap = function(mode, lhs, rhs)
			vim.keymap.set(mode, lhs, rhs, { buffer = true })
		end

		bufmap("n", "K", vim.lsp.buf.hover)
		bufmap("n", "gd", vim.lsp.buf.definition)
		-- vim.keymap.set('n', 'gd', '<cmd>Telescope lsp_definitions<CR>', { desc = 'LSP Definitions' })

		-- vim.keymap.set('n', 'gr', '<cmd>Telescope lsp_references<CR>', { desc: 'LSP References' })
		bufmap("n", "gD", vim.lsp.buf.declaration)
		bufmap("n", "gi", vim.lsp.buf.implementation)
		bufmap("n", "go", vim.lsp.buf.type_definition)
		bufmap("n", "gr", vim.lsp.buf.references)
		bufmap("n", "gs", vim.lsp.buf.signature_help)
		bufmap("n", "cr", vim.lsp.buf.rename)
		-- bufmap("n", "cr", function()
		-- 	local curr_name = vim.fn.expand("<cword>")
		-- 	vim.ui.input({ prompt = "Rename to: ", default = curr_name }, function(new_name)
		-- 		if new_name and #new_name > 0 and new_name ~= curr_name then
		-- 			vim.lsp.buf.rename(new_name)
		-- 		end
		-- 	end)
		-- end)

		bufmap("n", "ca", vim.lsp.buf.code_action)
		bufmap("n", "cl", vim.diagnostic.open_float)
		bufmap("n", "[d", vim.diagnostic.goto_prev)
		bufmap("n", "]d", vim.diagnostic.goto_next)
	end,
})


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
		c        = { "clang-format", lsp_format = "fallback" },
		cpp        = { "clang-format", lsp_format = "fallback" },
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
