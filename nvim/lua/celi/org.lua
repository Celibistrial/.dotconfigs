-- init.lua

-- Load custom treesitter grammar for org filetype
require("orgmode").setup_ts_grammar()
require("org-bullets").setup()
-- Treesitter configuration
require("nvim-treesitter.configs").setup({
	-- If TS highlights are not enabled at all, or disabled via `disable` prop,
	-- highlighting will fallback to default Vim syntax highlighting
	highlight = {
		enable = true,
		-- Required for spellcheck, some LaTex highlights and
		-- code block highlights that do not have ts grammar
		additional_vim_regex_highlighting = { "org" },
	},
	ensure_installed = { "org" }, -- Or run :TSUpdate org
})

require("orgmode").setup({
	org_agenda_files = { "~/org/*" },
	org_default_notes_file = "~/org/refile.org",
	org_capture_templates = {
		t = {
			description = "Task",
			template = "* TODO %?\n  %u",
		},
		j = {
			description = "Journal",
			template = "\n*** %<%Y-%m-%d> %<%A>\n**** %U%?",
			target = "~/org/journal.org",
		},
	},
  org_indent_mode = "virtual_indent",
  ui = {
    virtual_indent = {
      handler = nil,
      --- or
      handler = function(buffer, start_line, end_line)
        -- ...
      end,
    },
  },
})
