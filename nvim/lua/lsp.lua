require("lazy-lsp").setup {
	excluded_servers = {
		"ccls",              -- prefer clangd
		"denols",            -- prefer eslint and tsserver
		"docker_compose_language_service", -- yamlls should be enough?
		"flow",              -- prefer eslint and tsserver
		"ltex",              -- grammar tool using too much CPU
		"quick_lint_js",     -- prefer eslint and tsserver
		"rnix",              -- archived on Jan 25, 2024
		"scry",              -- archived on Jun 1, 2023
		"tailwindcss",       -- associates with too many filetypes
		"nil_ls"
	},
	preferred_servers = {
		--    markdown = {},
		--    python = { "pyright", "ruff_lsp" },
		-- svelte = {"eslint","svelte"}
	},
	prefer_local = true, -- Prefer locally installed servers over nix-shell
	default_config = {
		flags = {
			debounce_text_changes = 150,
		},
		-- on_attach = on_attach,
		-- capabilities = capabilities,
	},
	-- Override config for specific servers that will passed down to lspconfig setup.
	-- Note that the default_config will be merged with this specific configuration so you don't need to specify everything twice.
	configs = {
		lua_ls = {
			settings = {
				Lua = {
					diagnostics = {
						-- Get the language server to recognize the `vim` global
						globals = { "vim" },
					},
				},
			},
		},
		nixd = {
			settings = {
				nixd = {
					formatting = {
						command = { "alejandra" },
					},
				},
			},
		},
	},
}


local cmp = require('cmp')
local cmp_action = require('lsp-zero').cmp_action()
require("luasnip.loaders.from_vscode").lazy_load({ paths = "./snippets" })
local luasnip = require("luasnip")
cmp.setup({
	sources = {
		{ name = 'nvim_lsp' },
		{ name = 'luasnip' },
	},
	mapping = {

		-- ... Your other mappings ...
		['<CR>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				if luasnip.expandable() then
					luasnip.expand()
				else
					cmp.confirm({
						select = true,
					})
				end
			else
				fallback()
			end
		end),

		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif luasnip.locally_jumpable(1) then
				luasnip.jump(1)
			else
				fallback()
			end
		end, { "i", "s" }),

		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif luasnip.locally_jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { "i", "s" }),

		-- ... Your other mappings ...
	},
	-- mapping = cmp.mapping.preset.insert({
	-- 	['<Tab>'] = cmp_action.luasnip_supertab(),
	-- 	['<S-Tab>'] = cmp_action.luasnip_shift_supertab(),
	-- 	['<C-Space>'] = cmp.mapping.complete(),
	-- }),
	snippet = {
		expand = function(args)
			require('luasnip').lsp_expand(args.body)
		end,
	},
})
local lsp_zero = require("lsp-zero")
lsp_zero.on_attach(function(client, bufnr)
	-- see :help lsp-zero-keybindings to learn the available actions
	lsp_zero.default_keymaps({
		buffer = bufnr,
	})
	local opts = { noremap = true, silent = true }
	local keymap = vim.api.nvim_buf_set_keymap
	keymap(bufnr, "n", "cD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
	keymap(bufnr, "n", "cd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
	keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
	keymap(bufnr, "n", "cI", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
	keymap(bufnr, "n", "<leader>cr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	keymap(bufnr, "n", "cl", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
	keymap(bufnr, "n", "cf", "<cmd>lua vim.lsp.buf.format{ async = true }<cr>", opts)
	-- keymap(bufnr, "n", "li", "<cmd>LspInfo<cr>", opts)
	-- keymap(bufnr, "n", "<leader>lI", "<cmd>LspInstallInfo<cr>", opts)
	keymap(bufnr, "n", "ca", "<cmd>lua vim.lsp.buf.code_action()<cr>", opts)
	keymap(bufnr, "n", "<leader>lj", "<cmd>lua vim.diagnostic.goto_next({buffer=0})<cr>", opts)
	keymap(bufnr, "n", "<leader>lk", "<cmd>lua vim.diagnostic.goto_prev({buffer=0})<cr>", opts)
	keymap(bufnr, "n", "cr", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
	keymap(bufnr, "n", "<leader>ls", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
	keymap(bufnr, "n", "<leader>lq", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)
end)
require('lspconfig').intelephense.setup({})
