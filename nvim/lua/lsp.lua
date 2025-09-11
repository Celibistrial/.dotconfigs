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
local cmp = require("cmp")
local luasnip = require("luasnip")


cmp.setup({
	snippet = {
		expand = function(args) luasnip.lsp_expand(args.body) end,
	},
	sources = {
		{ name = "path" },
		{ name = "nvim_lsp", keyword_length = 1 },
		{ name = "buffer",   keyword_length = 3 },
		{ name = "luasnip",  keyword_length = 2 },
	},
	window = {
		documentation = cmp.config.enable,
	},
	formatting = {
		fields = { "menu", "abbr", "kind" },
		format = function(entry, item)
			local menu_icon = {
				nvim_lsp = "󰘧", luasnip = " ", buffer = "󰬴 ", path = " ",
			}
			item.menu = menu_icon[entry.source.name]
			return item
		end,
	},
	mapping = {
		["<Up>"]    = cmp.mapping.select_prev_item(),
		["<Down>"]  = cmp.mapping.select_next_item(),
		["<C-p>"]   = cmp.mapping.select_prev_item(),
		["<C-n>"]   = cmp.mapping.select_next_item(),
		["<C-u>"]   = cmp.mapping.scroll_docs(-4),
		["<C-d>"]   = cmp.mapping.scroll_docs(4),
		["<C-e>"]   = cmp.mapping.abort(),
		["<C-y>"]   = cmp.mapping.confirm({ select = true }),
		["<CR>"]    = cmp.mapping.confirm({ select = false }),
		["<C-f>"]   = cmp.mapping(function(fallback)
			if luasnip.jumpable(1) then luasnip.jump(1) else fallback() end
		end, { "i", "s" }),
		["<C-b>"]   = cmp.mapping(function(fallback)
			if luasnip.jumpable(-1) then luasnip.jump(-1) else fallback() end
		end, { "i", "s" }),
		["<Tab>"]   = cmp.mapping(function(fallback)
			local col = vim.fn.col(".") - 1
			if cmp.visible() then
				cmp.select_next_item()
			elseif col == 0 or vim.fn.getline("."):sub(col, col):match("%s") then
				fallback()
			else
				cmp.complete()
			end
		end, { "i", "s" }),
		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then cmp.select_prev_item() else fallback() end
		end, { "i", "s" }),
	},
})

---------------------------------------------------------------------
-- 🌟 LSP Keymaps
---------------------------------------------------------------------
vim.api.nvim_create_autocmd("LspAttach", {

	callback = function()
		local bufmap = function(mode, lhs, rhs)
			vim.keymap.set(mode, lhs, rhs, { buffer = true })
		end

		bufmap("n", "K", vim.lsp.buf.hover)
		bufmap("n", "gd", vim.lsp.buf.definition)
		bufmap("n", "gD", vim.lsp.buf.declaration)
		bufmap("n", "gi", vim.lsp.buf.implementation)
		bufmap("n", "go", vim.lsp.buf.type_definition)
		bufmap("n", "gr", vim.lsp.buf.references)
		bufmap("n", "gs", vim.lsp.buf.signature_help)
		bufmap("n", "cr", function()
			local curr_name = vim.fn.expand("<cword>")
			vim.ui.input({ prompt = "Rename to: ", default = curr_name }, function(new_name)
				if new_name and #new_name > 0 and new_name ~= curr_name then
					vim.lsp.buf.rename(new_name)
				end
			end)
		end)

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
